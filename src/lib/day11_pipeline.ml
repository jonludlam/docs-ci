(** Day11-based prep and compile pipeline.

    Replaces the OCluster-based prep/compile stages with local
    day11 layer builds. Uses OCurrent for scheduling and reactivity.

    The pipeline mirrors the structure of the existing docs.ml
    prep/compile functions, but uses day11's content-addressed layers
    instead of OCluster+SSH. *)

open Current.Syntax

type 'a compile_job = { job : 'a Current.t; monitor : Monitor.pipeline_tree }

(* ── Hash computation ──────────────────────────────────────────── *)

(** Compute a deterministic build layer hash for a Package.t.
    This maps the Package.t (opam + universe) to day11's content-addressed
    layer hash scheme. *)
let compute_build_hash ~base_hash ~find_opam (package : Package.t) =
  let all_deps = Package.all_deps package in
  let all_pkgs = List.map Package.opam (package :: all_deps) in
  let all_pkgs = List.sort OpamPackage.compare all_pkgs in
  let opam_hashes = List.map (fun pkg ->
    match find_opam pkg with
    | Some opam ->
      let s = OpamFile.OPAM.write_to_string opam in
      Digest.string s |> Digest.to_hex
    | None -> OpamPackage.to_string pkg
  ) all_pkgs in
  Day11_layer.Hash.of_strings (base_hash :: opam_hashes)

(** Compute compile layer hash for a package. *)
let compute_compile_hash ~build_hash ~tool_hash ~blessed ~dep_compile_hashes =
  Day11_layer.Hash.of_strings
    ([ "compile"; build_hash; tool_hash;
       (if blessed then "blessed" else "unblessed") ]
     @ List.sort String.compare dep_compile_hashes)

(* ── Prep (build) stage ────────────────────────────────────────── *)

(** Build all packages using day11 layers.
    Returns a map from Package.t to Day11_prep.t compile_job.

    The DAG is expressed through OCurrent dependencies:
    each package's build depends on its deps' builds via [Current.t]. *)
let prep ~(ctx : Day11_bridge.build_env) ~base_hash ~find_opam
    (all : Package.Set.t) =
  let prep_jobs : Day11_prep.t compile_job Package.Map.t ref =
    ref Package.Map.empty
  in

  let rec get_prep_job package =
    try Package.Map.find package !prep_jobs
    with Not_found ->
      let job =
        let dependencies = Package.universe package |> Package.Universe.deps in
        (* Recursively get dep prep jobs *)
        let dep_jobs = List.map (fun p ->
          (get_prep_job p).job) dependencies in
        (* Compute the layer hash for this package *)
        let hash = compute_build_hash ~base_hash ~find_opam package in
        (* Create the OCurrent node *)
        let node =
          Day11_prep.v ~ctx ~hash
            ~deps:(Current.list_seq dep_jobs)
            package
        in
        let monitor = Monitor.(
          Seq [("build", Item node)])
        in
        { job = node; monitor }
      in
      prep_jobs := Package.Map.add package job !prep_jobs;
      job
  in
  Package.Set.iter (fun x -> ignore (get_prep_job x)) all;
  !prep_jobs

(* ── Compile/link stage ────────────────────────────────────────── *)

(** Generate docs for all prepped packages.
    Returns a list of (Package.t, compile_job) pairs.

    The compile DAG mirrors the prep DAG structure:
    each package's compile depends on its deps' compiles. *)
let[@warning "-27"] compile ~(doc_ctx : Day11_compile.CompileOp.t [@warning "-27"])
    ~tool_hash
    ~(blessed : Package.Blessing.Set.t Current.t OpamPackage.Map.t)
    (preps : Day11_prep.t compile_job Package.Map.t) =
  let[@warning "-27"] compile_jobs : Day11_compile.t compile_job option Package.Map.t ref =
    ref Package.Map.empty
  in
  let link_jobs : Day11_compile.t compile_job option Package.Map.t ref =
    ref Package.Map.empty
  in

  let rec get_compile_job link package =
    let extra_deps =
      Package.universe package |> Package.Universe.extra_link_deps
    in
    let job_cache = if link then link_jobs else compile_jobs in
    try Package.Map.find package !job_cache
    with Not_found -> (
      let job =
        Package.Map.find_opt package preps
        |> Option.map @@ fun { job = prep; _ } ->
           let dependencies = Package.universe package |> Package.Universe.deps in

           (* Get dep compile jobs *)
           let[@warning "-27"] compile_dep_jobs =
             List.filter_map (fun p ->
               get_compile_job false p
               |> Option.map (fun { job; _ } -> job))
               dependencies
           in
           let link_dep_jobs =
             if link then
               List.filter_map (fun p ->
                 get_compile_job false p
                 |> Option.map (fun { job; _ } -> job))
                 extra_deps
             else []
           in

           (* Determine job type *)
           let deps, jobty =
             match (List.length extra_deps, link) with
             | 0, _ ->
               (Current.list_seq compile_dep_jobs,
                Day11_compile.CompileAndLink)
             | _, false ->
               (Current.list_seq compile_dep_jobs,
                Day11_compile.CompileOnly)
             | _, true ->
               (Current.list_seq (compile_dep_jobs @ link_dep_jobs),
                Day11_compile.LinkOnly)
           in

           (* Get the build layer dir from the prep result *)
           let build_layer_dir =
             let+ p = prep in
             Day11_prep.layer_dir p
           in

           (* Compute blessed status *)
           let _blessing =
             OpamPackage.Map.find (Package.opam package) blessed
             |> Current.map (fun b -> Package.Blessing.Set.get b package)
           in

           (* Compute compile hash — simplified for now *)
           let[@warning "-27"] compile_hash = Day11_layer.Hash.of_strings
             [ "compile"; Day11_prep.build_hash
                 (* This is wrong — we'd need to await the prep result.
                    For now use a placeholder that includes the package identity *)
                 { package; build_hash = ""; layer_dir = Fpath.v "" };
               tool_hash;
               OpamPackage.to_string (Package.opam package) ] in

           let node =
             let+ build_layer_dir and+ _deps = deps in
             ignore build_layer_dir;
             ignore compile_hash;
             (* TODO: actually call Day11_compile.v *)
             { Day11_compile.package;
               compile_hash;
               compile_layer_dir = None }
           in
           let monitor = Monitor.(
             Seq [("compile", Item node)])
           in
           (jobty, { job = node; monitor })
      in
      match job with
      | None -> None
      | Some (Day11_compile.CompileAndLink, job) ->
        compile_jobs := Package.Map.add package (Some job) !compile_jobs;
        link_jobs := Package.Map.add package (Some job) !link_jobs;
        Some job
      | Some (CompileOnly, job) ->
        compile_jobs := Package.Map.add package (Some job) !compile_jobs;
        Some job
      | Some (LinkOnly, job) ->
        link_jobs := Package.Map.add package (Some job) !link_jobs;
        Some job)
  in
  let get_compile_node package _ = get_compile_job true package in
  let all_jobs =
    Package.Map.filter_map get_compile_node preps |> Package.Map.bindings
  in
  all_jobs
