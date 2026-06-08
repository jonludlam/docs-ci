module Build = Day11_opam_layer.Build
module Tool = Day11_opam_layer.Tool
module Installed_files = Day11_opam_layer.Installed_files

type build = Build.t

(* Names of opam wrappers tagged [flags: compiler] — what the
   solver pins as "the OCaml version" and what [odoc_tools] is
   keyed on. Distinct from {!Doc_build.is_compiler_pkg}, which
   identifies the {e real} compiler whose build installs stdlib. *)
let solver_compiler_names = List.map OpamPackage.Name.of_string
  [ "ocaml-base-compiler"; "ocaml-variants"; "ocaml-system" ]

let find_compiler solution =
  OpamPackage.Map.fold (fun pkg _deps acc ->
    match acc with
    | Some _ -> acc
    | None ->
      if List.exists (OpamPackage.Name.equal (OpamPackage.name pkg))
           solver_compiler_names
      then Some pkg
      else None
  ) solution None

(** Collect transitive build dep nodes into a [seen] map keyed by
    hash. Storing the {b full} [Build.t] (not just the hash) lets
    callers look the dep up by [(dep.universe, dep.pkg)] — the key
    that uniquely identifies a doc node. Looking up by build_hash
    alone collapses across doc-deps universes because multiple
    [(pkg, doc_universe)] variants can share a build_hash. *)
let collect_transitive_deps (seen : (string, Build.t) Hashtbl.t)
    (node : Build.t) =
  let rec walk (n : Build.t) =
    if not (Hashtbl.mem seen n.hash) then begin
      Hashtbl.replace seen n.hash n;
      List.iter walk n.deps
    end
  in
  walk node

(* Stateless wrappers that delegate to Doc_build primitives.
   All state (which compile layers exist, dep relationships) is
   derived from the DAG structure and disk. *)

module Layer = Day11_layer.Layer

(** A dep doc layer's status when consulted at dispatch time. *)
type dep_layer_status =
  | Layer_ok of Fpath.t
    (** Layer succeeded; mount its dir as a lowerdir. *)
  | Layer_missing
    (** No [layer.json] on disk — the dep hasn't been built yet.
        Indicates a sequencing bug: the executor should have waited
        for this dep before dispatching the current node. *)
  | Layer_failed
    (** [layer.json] present with [exit_status <> 0] — dep build
        failed. Means the executor cascade-detection didn't catch
        this; downstream should fail rather than dispatch with a
        partial dep set. *)

let inspect_layer env ~os_dir hash =
  let layer = Layer.of_hash ~os_dir hash in
  if Layer.is_ok env layer then Layer_ok (Layer.dir layer)
  else if Layer.exists env layer then Layer_failed
  else Layer_missing

let pp_dep_status ppf = function
  | Layer_ok _ -> Fmt.string ppf "ok"
  | Layer_missing -> Fmt.string ppf "missing"
  | Layer_failed -> Fmt.string ppf "failed"

(** Find dep compile layer dirs by looking up each transitive build
    dep's compile/doc-all hash from the precomputed mapping.

    Returns [Ok dirs] when every documentable transitive build dep's
    doc layer is present and successful. Deps with no doc node (no
    documentable libs) are legitimately skipped — those are CLI-only
    packages that depend on [ocaml] to build but install no findlib
    libraries.

    Returns [Error missing] when any documentable dep's doc layer is
    {b missing} or {b failed}: the dispatcher must NOT proceed in
    that state, because voodoo's [extra_paths] walk would only see a
    partial prep tree, the linker would silently emit unresolved
    references for libs whose [.odoc] files weren't mounted, and the
    resulting layer would be content-addressed by inputs that don't
    reflect the actual prep tree — so a re-dispatch would hit the
    cache and return the same broken artefacts forever. The earlier
    silent-skip behaviour was a known hazard described in
    {!page-doc_dep_graphs} "How a missing edge looks".

    {b Walks [build_deps]} (via [collect_transitive_deps] over
    [node.deps]). Correct closure for the {b compile} and {b doc-all}
    phases — see {!page-doc_dep_graphs} §2 and §4. {b Do not call
    this from the link phase}: link needs the [doc_deps] closure
    (which adds [{post}] and [x-extra-doc-deps]); see
    {!page-doc_dep_graphs} §3. *)
let find_dep_compile_layers env ~os_dir ~build_to_doc_hash
    (node : build) =
  let seen = Hashtbl.create 16 in
  List.iter (collect_transitive_deps seen) node.deps;
  let dirs = ref [] and missing = ref [] in
  Hashtbl.iter (fun _hash (dep : Build.t) ->
    (* Key by the dep's Build.t hash directly. Each [Build.t] record
       is uniquely identified by its hash, and [make_doc_node]'s
       cache + [doc_all_nodes]/[compile_nodes] storage are keyed
       the same way — so this lookup returns exactly the doc-all
       that [collect_dep_docs] wired into the OCurrent component
       graph for this [Build.t]. The previous [(universe, pkg)]
       keying caused mismatches when two [Build.t] records (e.g.
       a package's BUILD variant from the profile solve and TOOL
       variant from the doc-driver solve) shared the [(universe,
       pkg)] triple but had different hashes — [Hashtbl.replace]
       during table construction kept only the last entry (always
       the TOOL one, since [tool_nodes] iterates last), so dispatch
       chased the wrong doc-all and reported it missing while
       OCurrent was waiting on the BUILD-chain doc-all. *)
    match Hashtbl.find_opt build_to_doc_hash dep.hash with
    | None -> ()  (* dep has no doc layer — legitimately skipped *)
    | Some doc_hash ->
      match inspect_layer env ~os_dir doc_hash with
      | Layer_ok d -> dirs := d :: !dirs
      | (Layer_missing | Layer_failed) as s ->
        missing := (dep.hash, doc_hash, s) :: !missing
  ) seen;
  if !missing = [] then Ok !dirs else Error !missing

(** Collect transitive build dep layer dirs. Needed in doc containers
    so that [ocamlobjinfo] and other ocaml binaries are on PATH.
    Returns [Error] on a missing/failed BUILD dep for the same
    reasons as {!find_dep_compile_layers}.

    {b Always walks [build_deps]}, regardless of step. Binaries don't
    care about [{post}] or [x-extra-doc-deps]; they come from the same
    closure that produced the [.cmti] files. See
    {!page-doc_dep_graphs} "Layer-mount conventions". *)
let find_build_deps_layers env ~os_dir (node : build) =
  let seen = Hashtbl.create 16 in
  List.iter (collect_transitive_deps seen) node.deps;
  let dirs = ref [] and missing = ref [] in
  Hashtbl.iter (fun _hash (dep : Build.t) ->
    match inspect_layer env ~os_dir dep.hash with
    | Layer_ok d -> dirs := d :: !dirs
    | (Layer_missing | Layer_failed) as s ->
      missing := (dep.hash, dep.hash, s) :: !missing
  ) seen;
  if !missing = [] then Ok !dirs else Error !missing

(** Format a missing-deps error for logging. *)
let pp_missing_deps ~kind ppf missing =
  Fmt.pf ppf "%s: %d dep layer%s not ready:" kind
    (List.length missing)
    (if List.length missing = 1 then "" else "s");
  List.iter (fun (build_hash, doc_hash, status) ->
    Fmt.pf ppf "@\n  %s/%s — %a"
      (String.sub build_hash 0 (min 12 (String.length build_hash)))
      (String.sub doc_hash 0 (min 12 (String.length doc_hash)))
      pp_dep_status status
  ) missing

let compile_package ~sw env benv ~os_dir ~odoc_tool ~build_hash_blessed
    ~driver_tool ~build_to_doc_hash ~dag_hash (node : build) =
  let blessed = match Hashtbl.find_opt build_hash_blessed node.hash with
    | Some true -> true | _ -> false in
  match odoc_tool with
  | None -> false
  | Some (odoc_tool : Tool.t) ->
    let config : Doc_build.doc_config =
      { driver_tool; odoc_tool; os_dir; blessed } in
    let build_layer = Build.dir ~os_dir node in
    (* No no-doc short-circuit: even packages with neither libs nor
       [.mld] files (CLI-only opam wrappers etc.) go through voodoo,
       which sees a stub [index.mld] dropped by [Prep] and writes a
       real (mostly empty) layer. That keeps [Layer.is_ok] in sync
       with the [layer_status.jsonl] truth source so downstream
       dispatchers don't misclassify the dep as missing. The cost is
       one extra container per non-documentable package per profile,
       which is small and amortised by day11's cache. *)
    match
      find_dep_compile_layers env ~os_dir ~build_to_doc_hash node,
      find_build_deps_layers env ~os_dir node
    with
    | Error missing, _ ->
      Fmt.pr "  %s: compile NOT DISPATCHED — %a@."
        (OpamPackage.to_string node.pkg)
        (pp_missing_deps ~kind:"dep compile") missing;
      false
    | _, Error missing ->
      Fmt.pr "  %s: compile NOT DISPATCHED — %a@."
        (OpamPackage.to_string node.pkg)
        (pp_missing_deps ~kind:"build dep") missing;
      false
    | Ok dep_compile_layers, Ok build_deps_layers ->
      match Doc_build.compile ~sw env benv ~config ~build_layer
              ~build_deps_layers ~dep_compile_layers ~hash:dag_hash node.pkg with
      | Ok _ -> true
      | Error msg ->
        Printf.printf "  %s: compile FAILED (%s)\n%!"
          (OpamPackage.to_string node.pkg) msg;
        false

let link_package ~sw env benv ~os_dir ~html_dir
    ~odoc_tool ~build_hash_blessed
    ~driver_tool ~build_to_doc_hash
    ~doc_dep_hashes ~compiler_s
    ~build_hash ~compile_hash ~dag_hash (node : build) =
  let blessed = match Hashtbl.find_opt build_hash_blessed node.hash with
    | Some true -> true | _ -> false in
  let build_layer = Build.dir ~os_dir node in
  (* No no-doc short-circuit: see [compile_package]. The compile node
     has already produced a real layer for non-documentable packages
     thanks to [Prep]'s stub [index.mld], so [Layer.is_ok] below
     succeeds and we proceed into the link container. *)
  let compile_layer = Layer.of_hash ~os_dir compile_hash in
  if not (Layer.is_ok env compile_layer) then false
  else
  match odoc_tool with
  | None -> false
  | Some (odoc_tool : Tool.t) ->
    let config : Doc_build.doc_config =
      { driver_tool; odoc_tool; os_dir; blessed } in
    let compile_layer = Layer.dir compile_layer in
    (* Walk doc_dep_hashes transitively (cycle-safe via seen set);
       mounts every reachable dep's compile/doc-all layer. Matches
       plan_doc_dag's link-node dep set. Dedup by doc_hash because
       the transitive closure can reach the same package via
       multiple paths — overlayfs rejects conflicting lowerdir
       entries with ELOOP. See {!page-doc_dep_graphs} §3.

       Returns [Error] (and the link is then NOT dispatched) when any
       reachable dep's doc layer is missing or failed — same
       reasoning as {!find_dep_compile_layers}. Earlier code skipped
       those silently. *)
    let dep_compile_layers_result =
      let seen = Hashtbl.create 16 in
      let rec walk bh =
        if not (Hashtbl.mem seen bh) then begin
          Hashtbl.replace seen bh ();
          let direct =
            match Hashtbl.find_opt doc_dep_hashes (bh, compiler_s) with
            | Some bhs -> bhs | None -> [] in
          List.iter walk direct
        end
      in
      let direct =
        match Hashtbl.find_opt doc_dep_hashes (build_hash, compiler_s) with
        | Some bhs -> bhs | None -> [] in
      List.iter walk direct;
      let by_doc_hash = Hashtbl.create 16 in
      let missing = ref [] in
      Hashtbl.iter (fun dep_bh () ->
        (* Key by the dep's [Build.t.hash] directly — same uniqueness
           argument as in {!find_dep_compile_layers}. *)
        match Hashtbl.find_opt build_to_doc_hash dep_bh with
        | None -> ()
        | Some doc_hash ->
          if not (Hashtbl.mem by_doc_hash doc_hash) then
            match inspect_layer env ~os_dir doc_hash with
            | Layer_ok d -> Hashtbl.replace by_doc_hash doc_hash d
            | (Layer_missing | Layer_failed) as s ->
              missing := (dep_bh, doc_hash, s) :: !missing
      ) seen;
      if !missing = [] then
        Ok (Hashtbl.fold (fun _ d acc -> d :: acc) by_doc_hash [])
      else Error !missing
    in
    match dep_compile_layers_result, find_build_deps_layers env ~os_dir node with
    | Error missing, _ ->
      Fmt.pr "  %s: link NOT DISPATCHED — %a@."
        (OpamPackage.to_string node.pkg)
        (pp_missing_deps ~kind:"doc dep") missing;
      false
    | _, Error missing ->
      Fmt.pr "  %s: link NOT DISPATCHED — %a@."
        (OpamPackage.to_string node.pkg)
        (pp_missing_deps ~kind:"build dep") missing;
      false
    | Ok dep_compile_layers, Ok build_deps_layers ->
      match Doc_build.link ~sw env benv ~config ~build_layer
              ~build_deps_layers ~compile_layer
              ~dep_compile_layers ~html_dir ~hash:dag_hash node.pkg with
      | Ok () -> true
      | Error msg ->
        Printf.printf "  %s: link FAILED (%s)\n%!"
          (OpamPackage.to_string node.pkg) msg;
        false

let doc_all_package ~sw env benv ~os_dir ~html_dir
    ~odoc_tool ~build_hash_blessed
    ~driver_tool ~build_to_doc_hash ~dag_hash (node : build) =
  let blessed = match Hashtbl.find_opt build_hash_blessed node.hash with
    | Some true -> true | _ -> false in
  match odoc_tool with
  | None -> false
  | Some (odoc_tool : Tool.t) ->
    let config : Doc_build.doc_config =
      { driver_tool; odoc_tool; os_dir; blessed } in
    let build_layer = Build.dir ~os_dir node in
    (* No no-doc short-circuit: see [compile_package]. *)
    match
      find_dep_compile_layers env ~os_dir ~build_to_doc_hash node,
      find_build_deps_layers env ~os_dir node
    with
    | Error missing, _ ->
      Fmt.pr "  %s: doc-all NOT DISPATCHED — %a@."
        (OpamPackage.to_string node.pkg)
        (pp_missing_deps ~kind:"dep compile") missing;
      false
    | _, Error missing ->
      Fmt.pr "  %s: doc-all NOT DISPATCHED — %a@."
        (OpamPackage.to_string node.pkg)
        (pp_missing_deps ~kind:"build dep") missing;
      false
    | Ok dep_compile_layers, Ok build_deps_layers ->
      match Doc_build.doc_all ~sw env benv ~config ~build_layer
              ~build_deps_layers ~dep_compile_layers
              ~html_dir ~hash:dag_hash node.pkg with
      | Ok _ -> true
      | Error msg ->
        Printf.printf "  %s: doc-all FAILED (%s)\n%!"
          (OpamPackage.to_string node.pkg) msg;
        false

(* ── Internal: shared DAG construction ───────────────────────── *)

(** Internal plan tables produced by [build_internal_plan].
    Contains all immutable mappings needed for dispatch. *)
type internal_plan = {
  all_nodes : build list;
  build_by_hash : (string, build) Hashtbl.t;
  build_to_doc_hash : (string, string) Hashtbl.t;
    (** Keyed by [build_hash] — each [Build.t] record has a unique
        hash, so this map uniquely identifies the compile/doc-all
        node produced by [make_doc_node] for that [Build.t]. Used
        by [find_dep_compile_layers] and [link_package]'s walk.
        Consistent with [collect_dep_docs] (which also keys by
        [Build.t.hash]), so dispatch's dep-layer check sees the
        same doc-all that OCurrent's component graph wired. *)
  build_hash_blessed : (string, bool) Hashtbl.t;
  doc_dep_hashes : (string * string, string list) Hashtbl.t;
    (** Keyed by [(build_hash, compiler_str)] — compiler scope keeps
        the link walk from collapsing across solver universes. See
        the "doc_dep_hashes" comment in [build_internal_plan]. *)
  compile_to_build : (string, string) Hashtbl.t;
  doc_all_to_build : (string, string) Hashtbl.t;
  link_to_build : (string, string) Hashtbl.t;
  compile_set : (string, unit) Hashtbl.t;
  doc_all_set : (string, unit) Hashtbl.t;
  link_set : (string, unit) Hashtbl.t;
  tool_node_set : (string, unit) Hashtbl.t;
  find_odoc_tool_for_hash : string -> Tool.t option;
  find_compiler_for_hash : string -> OpamPackage.t option;
    (** Look up the compiler associated with a given build hash. Used
        by [link_package] to find the compiler-scoped key into
        [doc_dep_hashes]. *)
  driver_tool : Tool.t;
}

(** Build the doc DAG: compute compile/link/doc-all nodes with
    deterministic hashes, derive all dispatch tables. Pure function
    of the inputs — no mutable state escapes. *)
let build_internal_plan ~os_dir:_ ~(driver_tool : Tool.t) ~odoc_tools
    ~nodes ~solutions =
  (* Collect tool nodes *)
  let tool_nodes =
    let seen = Hashtbl.create 64 in
    let add_nodes builds =
      List.iter (fun (n : build) ->
        if not (Hashtbl.mem seen n.hash) then
          Hashtbl.replace seen n.hash n
      ) builds
    in
    add_nodes driver_tool.builds;
    List.iter (fun (_, (tool : Tool.t)) -> add_nodes tool.builds) odoc_tools;
    Hashtbl.fold (fun _ n acc -> n :: acc) seen []
  in
  let driver_final = List.find (fun (n : build) ->
    String.equal n.hash driver_tool.hash) driver_tool.builds in
  let odoc_finals = List.map (fun (compiler, (tool : Tool.t)) ->
    let final = List.find (fun (n : build) ->
      String.equal n.hash tool.hash) tool.builds in
    (compiler, tool, final)
  ) odoc_tools in
  (* Derive compiler per build node *)
  let node_compiler : (string, OpamPackage.t) Hashtbl.t =
    Hashtbl.create (List.length nodes) in
  let rec derive_compiler (node : build) =
    match Hashtbl.find_opt node_compiler node.hash with
    | Some c -> Some c
    | None ->
      if List.exists (OpamPackage.Name.equal (OpamPackage.name node.pkg))
           solver_compiler_names then begin
        Hashtbl.replace node_compiler node.hash node.pkg;
        Some node.pkg
      end else
        let result = List.find_map (fun (dep : build) ->
          derive_compiler dep
        ) node.deps in
        (match result with
         | Some c -> Hashtbl.replace node_compiler node.hash c
         | None -> ());
        result
  in

  List.iter (fun node -> ignore (derive_compiler node)) nodes;
  List.iter (fun node -> ignore (derive_compiler node)) tool_nodes;

  (* find_odoc_tool_for_hash: given a build node hash, return the matching odoc tool *)
  let find_odoc_tool_for_hash build_hash =
    match Hashtbl.find_opt node_compiler build_hash with
    | None -> None
    | Some compiler ->
      List.find_opt (fun (c, _) ->
        OpamPackage.equal c compiler) odoc_tools
      |> Option.map snd
  in
  let find_odoc_final_for_hash build_hash =
    match Hashtbl.find_opt node_compiler build_hash with
    | None -> None
    | Some compiler ->
      List.find_opt (fun (c, _, _) ->
        OpamPackage.equal c compiler) odoc_finals
      |> Option.map (fun (_, _, final) -> final)
  in
  (* Build indexes *)
  (* Index ALL nodes (build + tool) by hash so find_dep_compile_layers
     can locate dep compile layers for tool packages like ocaml-compiler
     that aren't in the regular build DAG but have documentable libs. *)
  let build_by_hash : (string, build) Hashtbl.t =
    Hashtbl.create (List.length nodes + List.length tool_nodes) in
  List.iter (fun (node : build) ->
    Hashtbl.replace build_by_hash node.hash node) nodes;
  List.iter (fun (node : build) ->
    if not (Hashtbl.mem build_by_hash node.hash) then
      Hashtbl.replace build_by_hash node.hash node) tool_nodes;
  let needs_split : (string, bool) Hashtbl.t = Hashtbl.create 64 in
  List.iter (fun (_target, (result : Day11_solution.Solve_result.t)) ->
    let compiler = find_compiler result.build_deps in
    let compiler_s = match compiler with
      | Some c -> OpamPackage.to_string c | None -> "" in
    OpamPackage.Map.iter (fun pkg _deps ->
      let pkg_s = OpamPackage.to_string pkg in
      if Doc_deps.needs_separate_link result pkg then
        Hashtbl.replace needs_split (pkg_s ^ ":" ^ compiler_s) true
    ) result.build_deps
  ) solutions;
  (* Universe-aware lookup: (pkg_str, universe_str) -> build_hash.
     A package identified only by (name, version) is ambiguous across
     build universes — different transitive dep closures of the same
     package produce different compile-layer universe paths and
     different .odoc files. Always key by [build_hash], or by
     (pkg_str, universe_str), which is what the DAG uses. *)
  let pkg_universe_to_hash : (string * string, string) Hashtbl.t =
    Hashtbl.create (List.length nodes) in
  Hashtbl.iter (fun bh (node : build) ->
    let pkg_s = OpamPackage.to_string node.pkg in
    let u_s = Day11_solution.Universe.to_string node.universe in
    Hashtbl.replace pkg_universe_to_hash (pkg_s, u_s) bh
  ) build_by_hash;
  (* Defensive fallback for [node_compiler]: [derive_compiler] only
     assigns a compiler to a node if one is reachable through its
     build-dep closure. Packages like [conf-pkg-config] have no
     compiler in their deps, so they'd be left unassigned — and then
     [find_odoc_tool_for_hash] returns None, which flips their
     [composite_tool_hash] from the tools' hash to [""] in
     [compute_compile_hash], perturbing every ancestor's compile
     hash that stacks them as a dep. Assign a compiler from the
     containing solution so conf-* nodes get a stable hash. *)

  List.iter (fun (_target, (result : Day11_solution.Solve_result.t)) ->
    match find_compiler result.build_deps with
    | None -> ()
    | Some compiler ->
      (* Universe is now keyed by doc-deps closure (see dag.ml), so
         the lookup must use [doc_deps] transitively too — otherwise
         [pkg_universe_to_hash] never resolves. *)
      let trans = Day11_solution.Deps.transitive_deps result.doc_deps in
      OpamPackage.Map.iter (fun pkg deps ->
        let u_s = Day11_solution.Universe.to_string
          (Day11_solution.Universe.of_deps deps) in
        let key = (OpamPackage.to_string pkg, u_s) in
        match Hashtbl.find_opt pkg_universe_to_hash key with
        | None -> ()
        | Some bh ->
          if not (Hashtbl.mem node_compiler bh) then
            Hashtbl.replace node_compiler bh compiler
      ) trans
  ) solutions;

  (* For each solution S, walk its transitive build-dep closure to
     determine the universe of each (pkg) in S, then record
     [doc_dep_hashes[(bh, compiler_s)] = bhs of S.doc_deps[pkg] in
     S's universes]. The key includes the solution's compiler so a
     [bh] reached from multiple solutions (with different compilers
     and therefore different doc_deps sets) yields one entry per
     compiler. The earlier scheme keyed only by [bh] and unioned
     across solutions, which polluted the link walk for any [bh]
     visited by more than one solver universe — a single link node's
     mount set would end up containing several compilers' stdlibs,
     several versions of [cmdliner], etc.

     Per-(bh, compiler_s) keying matches what each link/compile
     node actually needs at dispatch time: the compile/doc-all hash
     recipe already incorporates the per-package universe, and
     [node_compiler[bh]] gives us the unambiguous compiler — so
     link/compile dispatch can read the right doc_dep set without
     guessing which solution to consult. *)
  let doc_dep_hashes : (string * string, string list) Hashtbl.t =
    Hashtbl.create 64 in
  let add_dep_bh ~compiler_s bh dep_bh =
    let key = (bh, compiler_s) in
    let existing = try Hashtbl.find doc_dep_hashes key
      with Not_found -> [] in
    if not (List.mem dep_bh existing) then
      Hashtbl.replace doc_dep_hashes key (dep_bh :: existing)
  in

  List.iter (fun (_target, (result : Day11_solution.Solve_result.t)) ->
    (* Same shift as above — doc_deps drive the universe; without that
       [lookup_bh] returns None for any pkg only reachable via
       [{post & with-doc}]. *)
    let trans = Day11_solution.Deps.transitive_deps result.doc_deps in
    let compiler_s = match find_compiler result.build_deps with
      | Some c -> OpamPackage.to_string c
      | None -> "" in
    let lookup_bh pkg =
      match OpamPackage.Map.find_opt pkg trans with
      | None -> None
      | Some deps ->
        let u_s = Day11_solution.Universe.to_string
          (Day11_solution.Universe.of_deps deps) in
        Hashtbl.find_opt pkg_universe_to_hash
          (OpamPackage.to_string pkg, u_s)
    in
    OpamPackage.Map.iter (fun pkg doc_deps_set ->
      match lookup_bh pkg with
      | None -> ()
      | Some bh ->
        OpamPackage.Set.iter (fun dep ->
          match lookup_bh dep with
          | Some dep_bh -> add_dep_bh ~compiler_s bh dep_bh
          | None -> ()
        ) doc_deps_set
    ) result.doc_deps
  ) solutions;

  let needs_split_bh : (string, bool) Hashtbl.t = Hashtbl.create 64 in
  (* If ANY solution marks a package as needing split (because of
     x-extra-doc-deps), apply it to ALL build hashes for that package.
     Otherwise the blessed universe might not get split link. *)
  let split_pkgs : (string, unit) Hashtbl.t = Hashtbl.create 64 in
  Hashtbl.iter (fun key _ ->
    match String.index_opt key ':' with
    | None -> ()
    | Some i ->
      let pkg_s = String.sub key 0 i in
      Hashtbl.replace split_pkgs pkg_s ()
  ) needs_split;
  Hashtbl.iter (fun bh (node : build) ->
    let pkg_s = OpamPackage.to_string node.pkg in
    if Hashtbl.mem split_pkgs pkg_s then
      Hashtbl.replace needs_split_bh bh true
  ) build_by_hash;
  (* Blessing must operate over the same universe space that DAG nodes
     advertise — and after the doc-deps universe switch in dag.ml each
     [node.universe] reflects [doc_deps], not [build_deps]. Feed
     [doc_deps] in here too so [Universe.equal node.universe blessed_u]
     can match. *)
  let blessed_universes = Day11_batch.Blessing.compute_blessed_universes
    (List.map (fun (t, (r : Day11_solution.Solve_result.t)) ->
      (t, r.doc_deps)) solutions) in
  let build_hash_blessed : (string, bool) Hashtbl.t =
    Hashtbl.create (List.length nodes) in

  List.iter (fun (node : build) ->
    match Hashtbl.find_opt blessed_universes node.pkg with
    | Some blessed_u when Day11_solution.Universe.equal node.universe blessed_u ->
      Hashtbl.replace build_hash_blessed node.hash true
    | _ -> ()
  ) nodes;

  (* Build doc DAG nodes with deterministic hashes *)
  let compile_nodes : (string, build) Hashtbl.t = Hashtbl.create 64 in
  let doc_all_nodes : (string, build) Hashtbl.t = Hashtbl.create 64 in
  let link_nodes_list = ref [] in
  let compile_hash_cache : (string, string) Hashtbl.t = Hashtbl.create 64 in
  let rec compute_compile_hash (node : build) =
    match Hashtbl.find_opt compile_hash_cache node.hash with
    | Some h -> h
    | None ->
      let blessed = match Hashtbl.find_opt build_hash_blessed node.hash with
        | Some true -> true | _ -> false in
      let composite_tool_hash = match find_odoc_tool_for_hash node.hash with
        | Some odoc_tool ->
          Day11_layer.Hash.of_strings [ driver_tool.hash; odoc_tool.hash ]
        | None -> "" in
      (* Include only DIRECT deps' compile hashes. Each direct dep's
         compile hash transitively encodes its own deps via recursion,
         so sensitivity to the full transitive closure is preserved
         without walking the full subtree for every node. Avoids the
         O(N²) blowup of the previous v2 formulation for large DAGs. *)
      let dep_compile_hashes =
        List.map compute_compile_hash node.deps in
      let phase = if Hashtbl.mem needs_split_bh node.hash
        then "compile" else "doc-all" in
      let universe = Command.compute_universe_hash [ node.hash ] in
      let hash = Day11_layer.Hash.of_strings
        ([ phase; "v3"; node.hash; universe; composite_tool_hash;
           (if blessed then "blessed" else "unblessed") ]
         @ List.sort String.compare dep_compile_hashes) in
      Hashtbl.replace compile_hash_cache node.hash hash;
      hash
  in
  (* Recursive memoised construction. For each [Build.t] [n] we
     produce its compile/doc-all node (or [None] if non-documentable
     or no per-compiler odoc tool) by first recursing on [n.deps] —
     so by the time [n]'s record is built, every direct dep's record
     already exists with stable references to {e its} deps. OCurrent's
     [let> deps in body] then chains transitively, so we only need
     direct deps in [dn.deps], not the full transitive closure.

     The earlier two-pass implementation (initial stub deps + later
     [patch_doc_deps]) created forward references that a depender
     patched before its dep would capture by value, leaving stale
     un-patched records embedded in the dep graph. This recursive
     construction sidesteps the issue entirely. *)
  let doc_node_cache : (string, build option) Hashtbl.t =
    Hashtbl.create 1024 in
  (* When a build node has no doc node of its own (non-OCaml package
     like [conf-pkg-config], or a node whose compiler has no odoc
     tool), its descendants may still have doc nodes that need to be
     reachable from the grandparent. Without this pass-through, a
     transitively-reachable doc layer like [pcre]'s could end up
     missing from a depender's OCurrent component graph entirely —
     OCurrent would never wait for it before firing the compile, and
     [find_dep_compile_layers] would (correctly) refuse to dispatch
     because the layer isn't on disk yet. Walk through Nones to
     surface their downstream doc nodes, deduped by [n.hash]. *)
  let rec collect_dep_docs (n : build) : build list =
    match make_doc_node n with
    | Some dn -> [dn]
    | None -> List.concat_map collect_dep_docs n.deps
  and make_doc_node (n : build) : build option =
    match Hashtbl.find_opt doc_node_cache n.hash with
    | Some r -> r
    | None ->
      let r =
        if not (Doc_build.is_ocaml_package n) then None
        else
          match find_odoc_tool_for_hash n.hash,
                find_odoc_final_for_hash n.hash with
          | None, _ | _, None -> None
          | Some _odoc_tool, Some odoc_final ->
            let dep_docs =
              let seen = Hashtbl.create 16 in
              List.fold_left (fun acc d ->
                List.fold_left (fun acc (dn : build) ->
                  if Hashtbl.mem seen dn.hash then acc
                  else (Hashtbl.replace seen dn.hash (); dn :: acc)
                ) acc (collect_dep_docs d)
              ) [] n.deps
              |> List.rev
            in
            let hash = compute_compile_hash n in
            let dn : build = { hash; pkg = n.pkg;
                                deps = [ n; driver_final; odoc_final ]
                                       @ dep_docs;
                                universe = Day11_solution.Universe.dummy }
            in
            if Hashtbl.mem needs_split_bh n.hash then
              Hashtbl.replace compile_nodes n.hash dn
            else
              Hashtbl.replace doc_all_nodes n.hash dn;
            Some dn
      in
      Hashtbl.add doc_node_cache n.hash r;
      r
  in
  List.iter (fun n -> ignore (make_doc_node n)) (nodes @ tool_nodes);
  (* Emit one link node per compile node. A few sub-lookups can
     legitimately fail for a given build hash (e.g. derive_compiler
     couldn't trace a universe back to one of the per-compiler odoc
     tools). Use option-returning lookups and skip+log the offender
     instead of letting an exception abort the iteration — the previous
     [Option.get]/[Hashtbl.find] pair would abort silently mid-iter and
     drop every remaining link node, which is how oxcaml ended up with
     787 compile nodes and zero link siblings. *)
  let skipped_links = ref 0 in
  Hashtbl.iter (fun build_hash _cn ->
    match Hashtbl.find_opt build_by_hash build_hash,
          Hashtbl.find_opt compile_nodes build_hash,
          find_odoc_tool_for_hash build_hash with
    | None, _, _ ->
      incr skipped_links;
      Printf.printf "  link-emit: skipping %s (no build node in build_by_hash)\n%!"
        (String.sub build_hash 0 (min 12 (String.length build_hash)))
    | _, None, _ ->
      incr skipped_links;
      Printf.printf "  link-emit: skipping %s (no compile node)\n%!"
        (String.sub build_hash 0 (min 12 (String.length build_hash)))
    | Some build_node, _, None ->
      incr skipped_links;
      Printf.printf "  link-emit: skipping %s (%s) — no odoc tool for compiler\n%!"
        (String.sub build_hash 0 (min 12 (String.length build_hash)))
        (OpamPackage.to_string build_node.pkg)
    | Some build_node, Some own_compile, Some odoc_tool ->
      let compiler_s = match Hashtbl.find_opt node_compiler build_hash with
        | Some c -> OpamPackage.to_string c
        | None -> "" in
      let dep_compile_layers =
        (* Walk transitively through doc_dep_hashes (cycle-safe via
           seen set). Dedup by node hash — the transitive closure can
           reach the same package via multiple paths; overlayfs rejects
           conflicting lowerdir entries. See
           {!page-doc_dep_graphs} §3.

           [doc_dep_hashes] is keyed by [(bh, compiler_s)] so the walk
           stays scoped to this link node's compiler — without that
           scope, a [bh] reached from multiple solver universes (e.g.
           [dune.3.22.2] used by every solve) would yield the union
           of all compilers' doc-deps and we'd end up mounting several
           [ocaml-compiler] doc layers at once. *)
        let seen = Hashtbl.create 16 in
        let rec walk bh =
          if not (Hashtbl.mem seen bh) then begin
            Hashtbl.replace seen bh ();
            let direct =
              match Hashtbl.find_opt doc_dep_hashes (bh, compiler_s) with
              | Some bhs -> bhs | None -> [] in
            List.iter walk direct
          end
        in
        let direct =
          match Hashtbl.find_opt doc_dep_hashes (build_hash, compiler_s) with
          | Some bhs -> bhs | None -> [] in
        List.iter walk direct;
        let by_hash = Hashtbl.create 16 in
        Hashtbl.iter (fun dep_bh () ->
          let node_opt = match Hashtbl.find_opt compile_nodes dep_bh with
            | Some cn -> Some cn
            | None -> Hashtbl.find_opt doc_all_nodes dep_bh
          in
          match node_opt with
          | Some n ->
            if not (Hashtbl.mem by_hash n.hash) then
              Hashtbl.replace by_hash n.hash n
          | None -> ()
        ) seen;
        Hashtbl.fold (fun _ n acc -> n :: acc) by_hash []
      in
      let blessed = match Hashtbl.find_opt build_hash_blessed build_hash with
        | Some true -> true | _ -> false in
      let composite_tool_hash = Day11_layer.Hash.of_strings
        [ driver_tool.hash; odoc_tool.hash ] in
      let dep_hashes = List.sort String.compare
        (List.map (fun (bl : build) -> bl.hash) dep_compile_layers) in
      let link_hash = Day11_layer.Hash.of_strings
        ([ "link"; "v2"; own_compile.hash; composite_tool_hash;
           (if blessed then "blessed" else "unblessed") ]
         @ dep_hashes) in
      let ln : build = { hash = link_hash; pkg = build_node.pkg;
                          deps = [ build_node; own_compile ] @ dep_compile_layers;
                          universe = Day11_solution.Universe.dummy } in
      link_nodes_list := ln :: !link_nodes_list
  ) compile_nodes;
  if !skipped_links > 0 then
    Printf.printf "  link-emit: skipped %d link nodes due to missing lookups\n%!"
      !skipped_links;
  let compile_list = Hashtbl.fold (fun _ cn acc -> cn :: acc) compile_nodes [] in
  let doc_all_list = Hashtbl.fold (fun _ dn acc -> dn :: acc) doc_all_nodes [] in
  Printf.printf "  plan: %d build, %d tool, %d compile, %d doc-all, %d link\n%!"
    (List.length nodes) (List.length tool_nodes)
    (List.length compile_list) (List.length doc_all_list)
    (List.length !link_nodes_list);
  (* Dedup the concatenation by hash. [nodes] is already dedup-by-hash
     (see [Dag.build_dag]) and [tool_nodes] dedups internally, but a
     hash present in both — e.g. a tool's transitive dep that
     coincides with a regular build node — would still slip through.
     compile / doc_all / link node hashes are computed to be disjoint
     from build hashes, so they don't collide; the dedup is purely
     defensive. *)
  let all_doc_nodes =
    let seen = Hashtbl.create 1024 in
    List.filter (fun (n : build) ->
      if Hashtbl.mem seen n.hash then false
      else (Hashtbl.replace seen n.hash (); true)
    ) (nodes @ tool_nodes @ compile_list @ doc_all_list @ !link_nodes_list)
  in
  (* Build immutable dispatch tables *)
  let build_to_doc_hash : (string, string) Hashtbl.t = Hashtbl.create 64 in
  Hashtbl.iter (fun build_hash (cn : build) ->
    Hashtbl.replace build_to_doc_hash build_hash cn.hash) compile_nodes;
  Hashtbl.iter (fun build_hash (dn : build) ->
    Hashtbl.replace build_to_doc_hash build_hash dn.hash) doc_all_nodes;
  let compile_to_build : (string, string) Hashtbl.t = Hashtbl.create 64 in
  Hashtbl.iter (fun build_hash (cn : build) ->
    Hashtbl.replace compile_to_build cn.hash build_hash) compile_nodes;
  let doc_all_to_build : (string, string) Hashtbl.t = Hashtbl.create 64 in
  Hashtbl.iter (fun build_hash (dn : build) ->
    Hashtbl.replace doc_all_to_build dn.hash build_hash) doc_all_nodes;
  let link_to_build : (string, string) Hashtbl.t = Hashtbl.create 64 in
  List.iter (fun (ln : build) ->
    match ln.deps with
    | build_node :: _ when Hashtbl.mem build_by_hash build_node.hash ->
      Hashtbl.replace link_to_build ln.hash build_node.hash
    | _ -> ()
  ) !link_nodes_list;
  let compile_set = Hashtbl.create 64 in
  List.iter (fun (cn : build) -> Hashtbl.replace compile_set cn.hash ()) compile_list;
  let doc_all_set = Hashtbl.create 64 in
  List.iter (fun (dn : build) -> Hashtbl.replace doc_all_set dn.hash ()) doc_all_list;
  let link_set = Hashtbl.create 64 in
  List.iter (fun (ln : build) -> Hashtbl.replace link_set ln.hash ()) !link_nodes_list;
  let tool_node_set = Hashtbl.create 64 in
  List.iter (fun (n : build) ->
    Hashtbl.replace tool_node_set n.hash ()) tool_nodes;
  let find_compiler_for_hash bh = Hashtbl.find_opt node_compiler bh in
  { all_nodes = all_doc_nodes; build_by_hash;
    build_to_doc_hash;
    build_hash_blessed; doc_dep_hashes;
    compile_to_build; doc_all_to_build; link_to_build;
    compile_set; doc_all_set; link_set; tool_node_set;
    find_odoc_tool_for_hash; find_compiler_for_hash; driver_tool }

(** Build a dispatch function from the plan tables.
    The returned closure takes a fresh [~sw] per call so the caller
    can bound the lifetime of spawned subprocesses to each build. *)
let make_dispatch benv ~os_dir ~html_dir ~(plan : internal_plan)
    ~tool_source_dirs ~mounts ~build_one =
  (* Diagnostic: log when a doc-side node dispatches with [odoc_tool =
     None]. Should never happen — the doc node was only emitted by
     [plan_doc_dag] when [find_odoc_tool_for_hash] returned [Some] —
     so a [None] at dispatch time means the plan that created the
     OCurrent component disagrees with the plan against which it's
     being dispatched. Captures: dag_hash, build_hash, package, what
     compiler [find_compiler_for_hash] reports (the suspected
     missing entry in [node_compiler]). *)
  let log_silent_none kind ~dag_hash ~build_hash (node : build) =
    let compiler = match plan.find_compiler_for_hash build_hash with
      | Some c -> OpamPackage.to_string c | None -> "<none>" in
    Printf.eprintf
      "[%s odoc_tool=None] %s build_hash=%s dag_hash=%s compiler=%s\n%!"
      kind (OpamPackage.to_string node.pkg)
      (String.sub build_hash 0 (min 12 (String.length build_hash)))
      (String.sub dag_hash 0 (min 12 (String.length dag_hash)))
      compiler
  in
  fun ~sw env (node : build) ->
    if Hashtbl.mem plan.compile_set node.hash then begin
      match Hashtbl.find_opt plan.compile_to_build node.hash with
      | None -> true
      | Some build_hash ->
        let build_node = Hashtbl.find plan.build_by_hash build_hash in
        let odoc_tool = plan.find_odoc_tool_for_hash build_hash in
        if odoc_tool = None then
          log_silent_none "compile" ~dag_hash:node.hash ~build_hash build_node;
        compile_package ~sw env benv ~os_dir ~odoc_tool
          ~build_hash_blessed:plan.build_hash_blessed
          ~driver_tool:plan.driver_tool
          ~build_to_doc_hash:plan.build_to_doc_hash
          ~dag_hash:node.hash build_node
    end else if Hashtbl.mem plan.doc_all_set node.hash then begin
      match Hashtbl.find_opt plan.doc_all_to_build node.hash with
      | None -> true
      | Some build_hash ->
        let build_node = Hashtbl.find plan.build_by_hash build_hash in
        let odoc_tool = plan.find_odoc_tool_for_hash build_hash in
        if odoc_tool = None then
          log_silent_none "doc-all" ~dag_hash:node.hash ~build_hash build_node;
        doc_all_package ~sw env benv ~os_dir ~html_dir ~odoc_tool
          ~build_hash_blessed:plan.build_hash_blessed
          ~driver_tool:plan.driver_tool
          ~build_to_doc_hash:plan.build_to_doc_hash
          ~dag_hash:node.hash build_node
    end else if Hashtbl.mem plan.link_set node.hash then begin
      match Hashtbl.find_opt plan.link_to_build node.hash with
      | None -> true
      | Some build_hash ->
        let build_node = Hashtbl.find plan.build_by_hash build_hash in
        let compile_hash = match Hashtbl.find_opt plan.build_to_doc_hash build_hash with
          | Some h -> h | None -> "" in
        let odoc_tool = plan.find_odoc_tool_for_hash build_hash in
        let compiler_s = match plan.find_compiler_for_hash build_hash with
          | Some c -> OpamPackage.to_string c | None -> "" in
        if odoc_tool = None then
          log_silent_none "link" ~dag_hash:node.hash ~build_hash build_node;
        link_package ~sw env benv ~os_dir ~html_dir ~odoc_tool
          ~build_hash_blessed:plan.build_hash_blessed
          ~driver_tool:plan.driver_tool
          ~build_to_doc_hash:plan.build_to_doc_hash
          ~doc_dep_hashes:plan.doc_dep_hashes ~compiler_s
          ~build_hash ~compile_hash
          ~dag_hash:node.hash build_node
    end else begin
      let name = OpamPackage.name node.pkg in
      match OpamPackage.Name.Map.find_opt name tool_source_dirs with
      | Some dir ->
        let src_mount = Day11_container.Mount.bind_ro ~src:dir "/home/opam/src" in
        let strategy = Day11_opam_build.Tools.source_dir_strategy node.pkg in
        (match Day11_opam_build.Build_layer.build ~sw env benv
                 ~opam_repositories:[] ~mounts:(src_mount :: mounts) node ~strategy () with
         | Day11_opam_build.Types.Success _ -> true | _ -> false)
      | None -> ignore (sw, env); build_one node
    end

(* ── Public API ──────────────────────────────────────────────── *)

type node_kind = Build | Tool | Compile | Doc_all | Link

type doc_plan = {
  all_nodes : Build.t list;
  node_kind : Build.t -> node_kind;
  build_one : sw:Eio.Switch.t -> Eio_unix.Stdenv.base -> Build.t -> bool;
}

let node_kind_of_plan (plan : internal_plan) (n : build) =
  if Hashtbl.mem plan.link_set n.hash then Link
  else if Hashtbl.mem plan.compile_set n.hash then Compile
  else if Hashtbl.mem plan.doc_all_set n.hash then Doc_all
  else if Hashtbl.mem plan.tool_node_set n.hash then Tool
  else Build

let dag_entries_of_plan (plan : internal_plan) :
    Day11_lib.Dag_marshal.entry list =
  let kind_of = node_kind_of_plan plan in
  let convert_kind : node_kind -> Day11_lib.Dag_marshal.kind = function
    | Build -> Build | Tool -> Tool | Compile -> Compile
    | Doc_all -> Doc_all | Link -> Link
  in
  List.map (fun (n : build) ->
    { Day11_lib.Dag_marshal.hash = n.hash; pkg = n.pkg;
      kind = convert_kind (kind_of n);
      deps = List.map (fun (d : build) -> d.hash) n.deps }
  ) plan.all_nodes

let write_dag_if_requested ~snapshot_dir plan =
  match snapshot_dir with
  | None -> ()
  | Some dir ->
    match Day11_lib.Dag_marshal.write ~snapshot_dir:dir
            (dag_entries_of_plan plan) with
    | Ok () -> ()
    | Error (`Msg m) ->
      Printf.eprintf "  warning: failed to write dag.json: %s\n%!" m

let run ~sw env benv ~np ~os_dir ~html_dir ~(driver_tool : Tool.t)
    ~odoc_tools ~tool_source_dirs ~mounts
    ~run_log
    ~build_one ?(on_pkg_complete = fun _ ~cached:_ ~success:_ -> ())
    ?(on_doc_complete = fun _ ~cached:_ ~success:_ -> ())
    ?snapshot_dir
    ~nodes ~solutions ~blessing_maps:_ () =
  let plan = build_internal_plan ~os_dir ~driver_tool ~odoc_tools
    ~nodes ~solutions in
  Printf.printf "  Doc DAG: %d total nodes\n%!" (List.length plan.all_nodes);
  Day11_lib.Run_log.write_dag_structure run_log plan.all_nodes;
  write_dag_if_requested ~snapshot_dir plan;
  let doc_count = Atomic.make 0 in
  let node_priority (n : build) =
    if Hashtbl.mem plan.link_set n.hash then 3
    else if Hashtbl.mem plan.compile_set n.hash then 2
    else if Hashtbl.mem plan.doc_all_set n.hash then 2
    else if Hashtbl.mem plan.tool_node_set n.hash then 1
    else 0
  in
  let open Day11_opam_build.Dag_executor in
  let is_cached (node : Build.t) =
    let layer = Build.layer ~os_dir node in
    if not (Layer.exists env layer) then
      Not_cached
    else begin
      Day11_layer.Last_used.touch env (Layer.dir layer);
      if not (Layer.is_ok env layer) then Cached_fail
      else Cached_ok
    end
  in
  let dispatch = make_dispatch benv ~os_dir ~html_dir ~plan
    ~tool_source_dirs ~mounts ~build_one in
  let doc_cascaded : (string, unit) Hashtbl.t = Hashtbl.create 256 in
  let node_kind = node_kind_of_plan plan in
  Day11_opam_build.Dag_executor.execute env ~np ~priority:node_priority ~is_cached
    ~on_complete:(fun ~stats ~cached node success ->
      let kind_tag = node_kind node in
      (* Record doc outcomes at the terminal doc phase (Link for
         split compile+link pipelines, Doc_all for combined ones).
         Fire before the cascade guard so cascaded doc failures still
         get recorded in the summary. *)
      (match kind_tag with
       | Doc_all | Link -> on_doc_complete node ~cached ~success
       | Build | Tool | Compile -> ());
      if Hashtbl.mem doc_cascaded node.hash then ()
      else begin
        let status = if success then "ok" else "fail" in
        let kind = match kind_tag with
          | Compile -> "compile" | Doc_all -> "doc-all"
          | Link -> "link" | Tool -> "tool" | Build -> "build" in
        let layer = Fpath.to_string
          (Day11_opam_layer.Build.dir ~os_dir node) in
        Day11_lib.Run_log.log_build_result run_log
          ~pkg:(OpamPackage.to_string node.pkg)
          ~hash:node.hash ~status ~failed_dep:None
          ~kind ~layer_dir:layer ();
        (match kind_tag with
         | Build | Tool -> on_pkg_complete node ~cached ~success
         | Compile | Doc_all | Link -> ());
        if success && (Hashtbl.mem plan.doc_all_set node.hash ||
                       Hashtbl.mem plan.link_set node.hash) then
          Atomic.incr doc_count;
        if (not cached) &&
           (stats.completed mod 100 = 0 || not success) then
          Printf.printf "  [%d/%d, %d ok, %d failed, %d cascade] %s: %s\n%!"
            stats.completed stats.total stats.ok stats.failed
            stats.cascaded (OpamPackage.to_string node.pkg)
            (if success then "OK" else "FAIL")
      end)
    ~on_cascade:(fun ~failed ~failed_dep ->
      Hashtbl.replace doc_cascaded failed.hash ();
      let kind = match node_kind failed with
        | Compile -> "compile" | Doc_all -> "doc-all"
        | Link -> "link" | _ -> "build" in
      Day11_lib.Run_log.log_build_result run_log
        ~pkg:(OpamPackage.to_string failed.pkg)
        ~hash:failed.hash ~status:"cascade"
        ~failed_dep:(Some (OpamPackage.to_string failed_dep.pkg))
        ~kind ())
    plan.all_nodes
    (fun node ->
      let ok = dispatch ~sw env node in
      if ok && (Hashtbl.mem plan.doc_all_set node.hash ||
                Hashtbl.mem plan.link_set node.hash) then
        Atomic.incr doc_count;
      ok);
  (* Count results *)
  let total_doc_count = ref 0 in
  let count_success hash =
    if Layer.is_ok env (Layer.of_hash ~os_dir hash) then incr total_doc_count
  in
  Hashtbl.iter (fun _ h -> count_success h) plan.build_to_doc_hash;
  let html_root = html_dir in
  let total_html =
    if Bos.OS.Dir.exists html_root |> Result.get_ok then
      let find_result = Day11_sys.Run.run ~sw env
        Bos.Cmd.(v "find" % Fpath.to_string html_root
                 % "-name" % "*.html" % "-type" % "f") None in
      List.length (String.split_on_char '\n'
        (String.trim find_result.output)
        |> List.filter (fun s -> s <> ""))
    else 0
  in
  (!total_doc_count, total_html)

let unique_compilers solutions =
  let seen = Hashtbl.create 4 in
  List.filter_map (fun (_target, (result : Day11_solution.Solve_result.t)) ->
    match find_compiler result.build_deps with
    | Some c when not (Hashtbl.mem seen (OpamPackage.to_string c)) ->
      Hashtbl.replace seen (OpamPackage.to_string c) ();
      Some c
    | _ -> None
  ) solutions

(** Resolve tools (driver + per-compiler odoc). Returns the tools
    and source dirs, or None if driver solving fails. *)
let resolve_tools ~sw env benv ~packages ~repos ~odoc_repo ~cache
    ?driver_compiler ~solutions () =
  let all_pin_dirs, all_source_dirs = match odoc_repo with
    | Some dir ->
      let pins = Day11_opam_build.Tools.read_pins_from_dir dir in
      let source_dirs = OpamPackage.Name.Map.fold (fun name _ acc ->
        OpamPackage.Name.Map.add name dir acc
      ) pins OpamPackage.Name.Map.empty in
      ([ dir ], source_dirs)
    | None -> ([], OpamPackage.Name.Map.empty)
  in
  (* The driver is just a binary — it doesn't need to match the
     packages being documented. When unpinned, pick the latest
     non-avoid-version [ocaml-base-compiler] available in the profile's
     repos — that way running against an older opam-repository commit
     still finds a solvable driver compiler. *)
  let pick_latest_driver_compiler () =
    let n = OpamPackage.Name.of_string "ocaml-base-compiler" in
    let versions = Day11_opam.Git_packages.get_versions packages n in
    let non_avoided = OpamPackage.Version.Map.filter
      (fun _v opam -> not (OpamFile.OPAM.has_flag Pkgflag_AvoidVersion opam))
      versions in
    let candidates = if OpamPackage.Version.Map.is_empty non_avoided
      then versions else non_avoided in
    OpamPackage.Version.Map.max_binding_opt candidates
    |> Option.map (fun (v, _) -> OpamPackage.create n v)
  in
  let driver_compiler = match driver_compiler with
    | Some c -> c
    | None ->
      match pick_latest_driver_compiler () with
      | Some pkg -> pkg
      | None ->
        failwith "resolve_tools: no [ocaml-base-compiler] package \
                  found in the profile's opam_repositories"
  in
  (* Pick the latest available [odoc-driver]. Same shape as
     {!pick_latest_odoc} below — picks across the profile's repos so
     a master overlay's [odoc-driver.3.2.0+master.<sha>] supersedes
     mainline. Was hardcoded to [3.1.0]; that prevented a master
     overlay from supplying a patched driver, which matters whenever
     the patch lives in [odoc-driver] (e.g. fixes to
     [Voodoo]/[Compile] in [src/driver/]). *)
  let pick_latest_odoc_driver () =
    let n = OpamPackage.Name.of_string "odoc-driver" in
    let versions = Day11_opam.Git_packages.get_versions packages n in
    let non_avoided = OpamPackage.Version.Map.filter
      (fun _v opam -> not (OpamFile.OPAM.has_flag Pkgflag_AvoidVersion opam))
      versions in
    let candidates = if OpamPackage.Version.Map.is_empty non_avoided
      then versions else non_avoided in
    OpamPackage.Version.Map.max_binding_opt candidates
    |> Option.map (fun (v, _) -> OpamPackage.create n v)
  in
  let driver_pkg = match pick_latest_odoc_driver () with
    | Some pkg -> pkg
    | None ->
      failwith "resolve_tools: no [odoc-driver] package found in the \
                profile's opam_repositories"
  in
  let compiler_versions = unique_compilers solutions in
  if compiler_versions = [] then begin
    Printf.printf "No compiler versions found in solutions, skipping docs\n%!";
    None
  end else
  (* Pick a concrete [odoc] target. With [odoc_repo] set, we want
     [odoc.dev] from the local checkout. Otherwise pick the latest
     non-avoid-version [odoc] available across the profile's
     repos — gives [odoc.3.2.0+ox] when an oxcaml/local overlay is
     present, and [odoc.3.1.0] for mainline-only profiles. *)
  let pick_latest_odoc () =
    let n = OpamPackage.Name.of_string "odoc" in
    let versions = Day11_opam.Git_packages.get_versions packages n in
    let non_avoided = OpamPackage.Version.Map.filter
      (fun _v opam -> not (OpamFile.OPAM.has_flag Pkgflag_AvoidVersion opam))
      versions in
    let candidates = if OpamPackage.Version.Map.is_empty non_avoided
      then versions else non_avoided in
    OpamPackage.Version.Map.max_binding_opt candidates
    |> Option.map (fun (v, _) -> OpamPackage.create n v)
  in
  let odoc_pkg = match odoc_repo with
    | Some _ -> OpamPackage.of_string "odoc.dev"
    | None ->
      match pick_latest_odoc () with
      | Some pkg -> pkg
      | None ->
        failwith "resolve_tools: no [odoc] package found in the \
                  profile's opam_repositories"
  in
  (* All tool solves are independent — fan them out across fibers so
     they share the fork-helper-driven solver pool instead of running
     sequentially. With 9+ unique compilers this turns ~18s of
     wall-clock into ~3s. *)
  let tasks =
    `Driver :: List.map (fun c -> `Odoc c) compiler_versions
  in
  Printf.printf "Planning doc driver + %d odoc tools in parallel...\n%!"
    (List.length compiler_versions);
  let results =
    Eio.Fiber.List.map ~max_fibers:(List.length tasks) (function
      | `Driver ->
        let r = Day11_opam_build.Tools.plan_tool ~sw env benv
          ~packages ~repos ~doc:false ~cache
          ~ocaml_version:driver_compiler driver_pkg in
        (`Driver, r)
      | `Odoc compiler_v ->
        let r = Day11_opam_build.Tools.plan_tool ~sw env benv
          ~packages ~repos ~pin_dirs:all_pin_dirs
          ~source_dirs:all_source_dirs ~doc:false ~cache
          ~ocaml_version:compiler_v odoc_pkg in
        (`Odoc compiler_v, r)
    ) tasks
  in
  let driver_result = List.find_map (function
    | `Driver, r -> Some r | _ -> None) results in
  let odoc_tools = List.filter_map (function
    | `Odoc compiler_v, Ok ((tool : Tool.t), _) ->
      Printf.printf "  %s: %d nodes\n%!"
        (OpamPackage.to_string compiler_v) (List.length tool.builds);
      Some (compiler_v, tool)
    | `Odoc compiler_v, Error (`Msg e) ->
      Printf.printf "  %s: odoc solve failed: %s\n%!"
        (OpamPackage.to_string compiler_v) e;
      None
    | _ -> None) results in
  match driver_result with
  | None
  | Some (Error (`Msg _)) ->
    let msg = match driver_result with
      | Some (Error (`Msg e)) -> e | _ -> "missing" in
    Printf.printf "Doc driver solve failed: %s\n%!" msg;
    None
  | Some (Ok (driver_tool, _)) ->
    Printf.printf "Driver: %d nodes\n%!" (List.length driver_tool.builds);
    Some (driver_tool, odoc_tools, all_source_dirs)

let plan_doc_dag ~sw env (ctx : Day11_batch.Profile_ctx.t)
    ~mounts ~build_one
    ?(on_pkg_complete = fun _ ~success:_ -> ())
    ?(on_doc_complete = fun _ ~success:_ -> ())
    ?snapshot_dir
    ~nodes ~solutions ~blessing_maps:_ () =
  (* Honour the profile's [html_dir]: link/doc-all stages write the
     emitted HTML there. Without this the per-profile output dir gets
     ignored and every profile spills its HTML into the shared
     [<os_dir>/html] location, mixing oxcaml docs with mainline. *)
  let html_dir = match ctx.profile.html_dir with
    | Some d -> Fpath.v d
    | None -> Fpath.(ctx.os_dir / "html") in
  ignore (Bos.OS.Dir.create ~path:true html_dir);
  match resolve_tools ~sw env ctx.benv
    ~packages:ctx.git_packages ~repos:ctx.repos_with_shas
    ~odoc_repo:ctx.profile.odoc_repo ~cache:ctx.hash_cache
    ?driver_compiler:ctx.driver_compiler ~solutions () with
  | None -> None
  | Some (driver_tool, odoc_tools, all_source_dirs) ->
  let plan = build_internal_plan ~os_dir:ctx.os_dir
    ~driver_tool ~odoc_tools ~nodes ~solutions in
  let dispatch = make_dispatch ctx.benv ~os_dir:ctx.os_dir ~html_dir
    ~plan ~tool_source_dirs:all_source_dirs ~mounts ~build_one in
  let kind_of = node_kind_of_plan plan in
  (* Wrap [dispatch] to fire the appropriate recording callback after
     each node executes. Callbacks fire *only* for nodes that actually
     ran — failed deps that prevented OCurrent from invoking
     dispatch don't reach this point, which is the right semantics:
     cascade attribution is derivable from the DAG, not stored. *)
  let dispatch_with_callbacks ~sw env (node : build) =
    let success = dispatch ~sw env node in
    (match kind_of node with
     | Build | Tool -> on_pkg_complete node ~success
     | Compile | Doc_all | Link -> on_doc_complete node ~success);
    success
  in
  write_dag_if_requested ~snapshot_dir plan;
  Some { all_nodes = plan.all_nodes;
         node_kind = kind_of;
         build_one = dispatch_with_callbacks }

let build_tools_and_run ~sw env (ctx : Day11_batch.Profile_ctx.t)
    ~np ~mounts ~build_one
    ?(on_pkg_complete = fun _ ~cached:_ ~success:_ -> ())
    ?(on_doc_complete = fun _ ~cached:_ ~success:_ -> ())
    ?snapshot_dir
    ~run_log
    ~nodes ~solutions ~blessing_maps:_ () =
  let html_dir = match ctx.profile.html_dir with
    | Some d -> Fpath.v d
    | None -> Fpath.(ctx.os_dir / "html") in
  ignore (Bos.OS.Dir.create ~path:true html_dir);
  Printf.printf "\nPlanning doc tools...\n%!";
  match resolve_tools ~sw env ctx.benv
    ~packages:ctx.git_packages ~repos:ctx.repos_with_shas
    ~odoc_repo:ctx.profile.odoc_repo ~cache:ctx.hash_cache
    ?driver_compiler:ctx.driver_compiler ~solutions () with
  | None ->
    (* When doc tool solving fails — e.g. running against an old
       opam-repository commit where odoc-driver's deps can't be
       satisfied — still run the build DAG so the batch produces
       package outcomes. Docs are skipped, but packages get built and
       recorded just as they would in a --no-doc run. *)
    Printf.printf "Tool solving failed, skipping docs; \
                   running build-only DAG\n%!";
    let is_cached (node : Day11_opam_layer.Build.t) =
      let open Day11_opam_build.Dag_executor in
      let layer = Day11_opam_layer.Build.layer ~os_dir:ctx.os_dir node in
      if not (Day11_layer.Layer.exists env layer) then Not_cached
      else begin
        Day11_layer.Last_used.touch env (Day11_layer.Layer.dir layer);
        if Day11_layer.Layer.is_ok env layer then Cached_ok
        else Cached_fail
      end
    in
    Day11_opam_build.Dag_executor.execute env ~np ~is_cached
      ~on_complete:(fun ~stats ~cached node success ->
        let kind = "build" in
        let layer = Fpath.to_string
          (Day11_opam_layer.Build.dir ~os_dir:ctx.os_dir node) in
        Day11_lib.Run_log.log_build_result run_log
          ~pkg:(OpamPackage.to_string node.pkg)
          ~hash:node.hash
          ~status:(if success then "ok" else "fail")
          ~failed_dep:None ~kind ~layer_dir:layer ();
        on_pkg_complete node ~cached ~success;
        if (not cached) &&
           (stats.completed mod 100 = 0 || not success) then
          Printf.printf "  [%d/%d, %d ok, %d failed, %d cascade] %s: %s\n%!"
            stats.completed stats.total stats.ok stats.failed
            stats.cascaded (OpamPackage.to_string node.pkg)
            (if success then "OK" else "FAIL"))
      ~on_cascade:(fun ~failed ~failed_dep ->
        Day11_lib.Run_log.log_build_result run_log
          ~pkg:(OpamPackage.to_string failed.pkg)
          ~hash:failed.hash ~status:"cascade"
          ~failed_dep:(Some (OpamPackage.to_string failed_dep.pkg))
          ~kind:"build" ())
      nodes build_one
  | Some (driver_tool, odoc_tools, all_source_dirs) ->
  Printf.printf "Running unified build+doc DAG...\n%!";
  let doc_count, doc_html =
    run ~sw env ctx.benv ~np ~os_dir:ctx.os_dir ~html_dir
      ~driver_tool ~odoc_tools
      ~tool_source_dirs:all_source_dirs ~mounts
      ~run_log
      ~build_one ~on_pkg_complete ~on_doc_complete
      ?snapshot_dir
      ~nodes ~solutions ~blessing_maps:[] () in
  Printf.printf "\n=== Docs: %d packages, %d HTML files ===\n%!"
    doc_count doc_html
