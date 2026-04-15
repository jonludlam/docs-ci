open Docs_ci_lib

let v ~config ~opam ~eio_env ~git_packages ~repos_with_shas
    ~html_dir () =
  let open Current.Syntax in
  let env = eio_env in
  let os_dir = Fpath.v (Sys.getenv_opt "DAY11_OS_DIR"
    |> Option.value ~default:"/var/cache/day11/debian-bookworm-x86_64") in
  let cache_dir = Fpath.v (Sys.getenv_opt "DAY11_CACHE_DIR"
    |> Option.value ~default:"/var/cache/day11") in
  ignore (Bos.OS.Dir.create ~path:true os_dir);
  ignore (Bos.OS.Dir.create ~path:true cache_dir);
  let base = match Day11_opam_build.Base.load_cached ~cache_dir
    ~os_distribution:"debian" ~os_version:"bookworm" with
  | Some b -> b
  | None ->
    Logs.err (fun f -> f "No cached base image. Run 'day11 batch' first.");
    failwith "No base image — run 'day11 batch' to create one"
  in
  let benv = Day11_opam_build.Types.make_build_env ~base ~os_dir () in
  let base_hash = Day11_opam_build.Base.build_hash
    ~os_distribution:"debian" ~os_version:"bookworm" ~arch:"x86_64" () in

  (* 1) Track packages *)
  let tracked =
    Track.v
      ~limit:(Config.take_n_last_versions config)
      ~filter:(Config.track_packages config)
      opam
  in

  (* 2) Solve *)
  let opam_repo = Sys.getenv_opt "DAY11_OPAM_REPO"
    |> Option.value ~default:(Sys.getenv "HOME" ^ "/ocaml/opam-repository") in
  let solutions = Day11_solver.solve ~opam_repo ~repos_with_shas
    ~opam_commit:opam tracked in
  let* solutions in

  Log.info (fun f -> f "Solved %d packages" (List.length solutions));

  (* 3) Build DAG from solutions *)
  let find_opam pkg =
    try Some (Day11_opam.Git_packages.get_package git_packages pkg)
    with Not_found -> None in
  let build_solutions = List.map (fun (s : Day11_solver.solution) ->
    (s.target, s.solve_result.build_deps)
  ) solutions in
  let hash_cache = Day11_opam_build.Hash_cache.create ~find_opam () in
  let nodes = Day11_opam_build.Dag.build_dag hash_cache
    ~base_hash build_solutions in
  Log.info (fun f -> f "%d build nodes" (List.length nodes));

  (* 4) Plan the doc DAG — tool solving + compile/link node construction.
     This runs synchronously (tool solving is fast) and returns the
     unified DAG nodes that we'll turn into OCurrent nodes below. *)
  let repo_mount = Day11_container.Mount.bind_ro
    ~src:opam_repo "/home/opam/.opam/repo/default" in
  let base_mounts =
    [ repo_mount ] @
    (match Day11_opam_build.Base.opam_build_mount ~cache_dir with
     | Some m -> [ m ] | None -> [])
  in
  let target_solutions = List.map (fun (s : Day11_solver.solution) ->
    (s.target, s.solve_result)
  ) solutions in
  let blessing_maps = List.map (fun (target, (result : Day11_solution.Solve_result.t)) ->
    (target, OpamPackage.Map.map (fun _ -> true) result.build_deps)
  ) target_solutions in
  let doc_plan = match html_dir with
    | None -> None
    | Some _ ->
      Day11_doc.Generate.plan_doc_dag benv ~os_dir
        ~packages:git_packages ~repos:repos_with_shas
        ~mounts:base_mounts ~odoc_repo:None
        ~build_one:(fun node ->
          match Day11_opam_build.Build_layer.build env benv
                  ~opam_repositories:[ Fpath.v opam_repo ]
                  ~mounts:base_mounts node () with
          | Day11_opam_build.Types.Success _ -> true
          | _ -> false)
        ~cache:hash_cache ~nodes
        ~solutions:target_solutions ~blessing_maps ()
  in

  (* 5) Create OCurrent nodes from the DAG.
     Use the doc plan's all_nodes if available, otherwise just build nodes.
     Each node becomes a separate OCurrent component visible in the web UI. *)
  let all_dag_nodes = match doc_plan with
    | Some plan ->
      Log.info (fun f -> f "Doc DAG: %d total nodes"
        (List.length plan.all_nodes));
      plan.all_nodes
    | None -> nodes
  in
  (* Single dispatch that handles all node types. For build-only mode
     (no docs), just build. With docs, the plan's dispatch handles
     build, tool, compile, link, and doc-all via internal classification. *)
  let dispatch = match doc_plan with
    | Some plan -> plan.build_one
    | None ->
      fun _env node ->
        match Day11_opam_build.Build_layer.build _env benv
                ~opam_repositories:[ Fpath.v opam_repo ]
                ~mounts:base_mounts node () with
        | Day11_opam_build.Types.Success _ -> true
        | _ -> false
  in
  let node_kind = match doc_plan with
    | Some plan -> plan.node_kind
    | None -> fun _ -> Day11_doc.Generate.Build
  in
  let pool = Current.Pool.create ~label:"day11-builds"
    (Config.jobs config) in
  let node_cache : (string, Day11_prep.t Current.t) Hashtbl.t =
    Hashtbl.create (List.length all_dag_nodes) in
  let rec make_node (dag_node : Day11_opam_layer.Build.t) =
    match Hashtbl.find_opt node_cache dag_node.hash with
    | Some existing -> existing
    | None ->
      let dep_currents = List.map make_node dag_node.deps in
      let deps = Current.list_seq dep_currents in
      let kind = node_kind dag_node in
      let label = match kind with
        | Day11_doc.Generate.Build -> "build"
        | Tool -> "tool"
        | Compile -> "compile"
        | Doc_all -> "doc"
        | Link -> "link"
      in
      let node =
        Day11_prep.run_node ~env ~os_dir ~pool ~dispatch
          ~label ~dag_node ~deps ()
      in
      Hashtbl.replace node_cache dag_node.hash node;
      node
  in
  let all_nodes = List.map make_node all_dag_nodes in
  let+ _results = Current.list_seq all_nodes in
  ()
