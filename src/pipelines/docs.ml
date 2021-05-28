open Docs_ci_lib

module CurrentMap (Map : OpamStd.MAP) = struct
  let map_seq (input : 'a Current.t Map.t) : 'a Map.t Current.t =
    Map.bindings input
    |> List.rev_map (fun (k, v) -> Current.map (fun x -> (k, x)) v)
    |> Current.list_seq |> Current.map Map.of_list
end

module OpamPackageNameCurrentMap = CurrentMap (OpamPackage.Name.Map)
module OpamPackageVersionCurrentMap = CurrentMap (OpamPackage.Version.Map)

module PrepStatus = struct
  type t = Jobs.t * Prep.t list Current_term.Output.t

  let pp f (t, _) = Fmt.pf f "Prep status %a" Jobs.pp t

  let compare (j1, r1) (j2, r2) =
    match Jobs.compare j1 j2 with
    | 0 ->
        Result.compare
          ~ok:(fun _ _ -> 0 (*twp same jobs yield the same prep list*))
          ~error:Stdlib.compare r1 r2
    | v -> v
end

let compile ~config ~voodoo ~cache ~(blessed : Package.Blessed.t Current.t OpamPackage.Map.t)
    (preps : Prep.t Current.t Package.Map.t) =
  let compilation_jobs = ref Package.Map.empty in

  let rec get_compilation_job package =
    try Package.Map.find package !compilation_jobs
    with Not_found ->
      let job =
        Package.Map.find_opt package preps
        |> Option.map @@ fun prep ->
           let dependencies = Package.universe package |> Package.Universe.deps in
           let compile_dependencies =
             List.filter_map get_compilation_job dependencies |> Current.list_seq
           in
           let blessed =
             OpamPackage.Map.find (Package.opam package) blessed
             |> Current.map (fun b -> Package.Blessed.is_blessed b package)
           in
           Compile.v ~config ~cache ~voodoo ~blessed ~deps:compile_dependencies prep
      in
      compilation_jobs := Package.Map.add package job !compilation_jobs;
      job
  in
  let get_compilation_node package _ = get_compilation_job package in
  Package.Map.filter_map get_compilation_node preps |> Package.Map.bindings

let blacklist = [ "ocaml-secondary-compiler"; "ocamlfind-secondary" ]

let take_any_success jobs =
  let open Current.Syntax in
  let* statuses = jobs |> List.map (Current.state ~hidden:true) |> Current.list_seq in
  let to_int = function
    | Ok _ -> 4
    | Error (`Active `Running) -> 3
    | Error (`Active `Ready) -> 2
    | Error (`Msg _) -> 1
  in
  let max a b = if to_int a >= to_int b then a else b in
  List.fold_left max (List.hd statuses) (List.tl statuses) |> Current.of_output

module StringMap = Map.Make (String)

let collapse_by ~key ~input (criteria : 'k -> string) (list : ('k * 'v Current.t) list) :
    ('k * 'v Current.t) list =
  let groups = ref StringMap.empty in
  List.iter
    (fun (pkg, value) ->
      groups :=
        StringMap.update (criteria pkg)
          (function None -> Some [ (pkg, value) ] | Some v -> Some ((pkg, value) :: v))
          !groups)
    list;

  !groups
  |> StringMap.mapi (fun k v ->
         let curr = List.map snd v in
         let keys = List.map fst v in
         let current, _ = Current.collapse_list ~key:(key ^ " " ^ k) ~value:"" ~input curr in
         List.combine keys current)
  |> StringMap.bindings |> List.rev_map snd |> List.flatten

let collapse_single ~key ~input list =
  let curr = List.map snd list in
  let keys = List.map fst list in
  let current, node = Current.collapse_list ~key ~value:"" ~input curr in
  (List.combine keys current, node)

let prep_hierarchical_collapse ~input lst =
  let key = "prep" in
  lst
  |> collapse_by ~key ~input (fun x -> x.Jobs.install |> Package.opam |> OpamPackage.name_to_string)
  |> collapse_by ~key ~input (fun x ->
         let name = x.Jobs.install |> Package.opam |> OpamPackage.name_to_string in
         String.sub name 0 1 |> String.uppercase_ascii)
  |> collapse_single ~key ~input

let compile_hierarchical_collapse ~input lst =
  let key = "compile" in
  lst
  |> collapse_by ~key ~input (Fmt.to_to_string Package.pp)
  |> collapse_by ~key ~input (fun x -> x |> Package.opam |> OpamPackage.to_string)
  |> collapse_by ~key ~input (fun x -> x |> Package.opam |> OpamPackage.name_to_string)
  |> collapse_by ~key ~input (fun x ->
         let name = x |> Package.opam |> OpamPackage.name_to_string in
         String.sub name 0 1 |> String.uppercase_ascii)
  |> collapse_single ~key ~input

let v ~config ~api ~opam () =
  let open Current.Syntax in
  let cache = Remote_cache.v (Config.ssh config) in
  let voodoo = Voodoo.v config in
  let v_do = Current.map Voodoo.Do.v voodoo in
  let v_prep = Current.map Voodoo.Prep.v voodoo in
  (* 1) Track the list of packages in the opam repository *)
  let metadata = Opam_metadata.v ~ssh:(Config.ssh config) ~repo:opam in
  let tracked =
    Track.v ~limit:(Config.take_n_last_versions config) ~filter:(Config.track_packages config) opam
  in
  (* 2) For each package.version, call the solver.  *)
  let solver_result = Solver.incremental ~config ~blacklist ~opam tracked in
  (* 3.a) From solver results, obtain a list of package.version.universe corresponding to prep jobs *)
  let* all_packages_jobs =
    solver_result |> Current.map (fun r -> Solver.keys r |> List.rev_map Solver.get)
  in
  (* 3.b) Expand that list to all the obtainable package.version.universe *)
  let all_packages =
    (* todo: add a append-only layer at this step *)
    all_packages_jobs |> List.rev_map Package.all_deps |> List.flatten
  in
  (* 4) Schedule a somewhat small set of jobs to obtain at least one universe for each package.version *)
  let jobs = Jobs.schedule ~targets:all_packages all_packages_jobs in
  (* 5) Run the preparation step *)
  let prepped, prepped_input_node =
    jobs
    |> List.map (fun job -> (job, Prep.v ~config ~cache ~voodoo:v_prep job))
    |> prep_hierarchical_collapse ~input:(Current.pair solver_result cache)
  in
  let prepped =
    prepped
    |> List.map (fun (job, result) ->
           job.Jobs.prep |> List.to_seq
           |> Seq.map (fun p -> (p, [ Current.map (Package.Map.find p) result ]))
           |> Package.Map.of_seq)
    |> List.fold_left (Package.Map.union (fun _ a b -> Some (a @ b))) Package.Map.empty
    |> Package.Map.map take_any_success
  in
  (* 6) Promote packages to the main tree *)
  let blessed =
    let by_opam_package =
      Package.Map.fold
        (fun package job opam_map ->
          let opam = Package.opam package in
          let job =
            let+ job = Current.state ~hidden:true job in
            (package, job)
          in
          OpamPackage.Map.update opam (List.cons job) [] opam_map)
        prepped OpamPackage.Map.empty
    in
    by_opam_package
    |> OpamPackage.Map.map (fun preps ->
           preps |> Current.list_seq
           |> Current.map (fun preps ->
                  (* We don't know yet about all preps status so we're optimistic here *)
                  preps
                  |> List.filter_map (function
                       | _, Error (`Msg _) -> None
                       | pkg, (Error (`Active _) | Ok _) -> Some pkg)
                  |> Package.Blessed.v))
  in

  (* 7) Odoc compile and html-generate artifacts *)
  let compiled, compiled_input_node =
    let c, cn =
      compile ~config ~cache ~voodoo:v_do ~blessed prepped
      |> compile_hierarchical_collapse ~input:prepped_input_node
    in
    (c |> List.to_seq |> Package.Map.of_seq, cn)
  in

  (* 8) Report status *)
  let package_registry =
    let+ tracked = tracked in
    List.iter
      (fun p ->
        Log.app (fun f -> f "Track: %s" (OpamPackage.to_string (Track.pkg p)));
        Web.register (Track.pkg p) api)
      tracked
  in
  let package_status =
    Package.Map.merge
      (fun _ a b -> match (a, b) with Some a, Some b -> Some (a, b) | _ -> None)
      prepped compiled
    |> Package.Map.mapi (fun package (prep_job, compile_job) ->
           let blessed = OpamPackage.Map.find (Package.opam package) blessed in
           let+ prep = prep_job |> Current.state ~hidden:true
           and+ compile = compile_job |> Current.state ~hidden:true
           and+ blessed = blessed in
           let blessed =
             if Package.Blessed.is_blessed blessed package then Docs_ci_lib.Web.Status.Blessed
             else Universe
           in
           match (prep, compile) with
           | Error (`Msg _), _ -> Web.Status.Failed
           | Error (`Active _), _ -> Pending Prep
           | _, Ok _ -> Success blessed
           | _, Error (`Msg _) -> Failed
           | _, Error (`Active _) -> Pending (Compile blessed))
  in
  let status =
    package_status
    |> Package.Map.mapi (fun package status ->
           Web.set_package_status ~package:(Current.return package) ~status api)
    |> Package.Map.bindings |> List.map snd |> Current.all
    |> Current.collapse ~input:compiled_input_node ~key:"Set status (graphql)" ~value:""
  in
  Current.all [ package_registry; status; metadata ]
