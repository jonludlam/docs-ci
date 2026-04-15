open Docs_ci_lib

let v ~config ~opam ~eio_env ~git_packages ~repos_with_shas () =
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

  (* 4) Build all packages as individual OCurrent nodes *)
  let build_ctx : Day11_bridge.build_env = {
    env; benv; os_dir; cache_dir;
    opam_repositories = [ Fpath.v opam_repo ];
  } in
  let node_cache : (string, Day11_prep.t Current.t) Hashtbl.t =
    Hashtbl.create (List.length nodes) in
  let rec build_node (dag_node : Day11_opam_layer.Build.t) =
    match Hashtbl.find_opt node_cache dag_node.hash with
    | Some existing -> existing
    | None ->
      let dep_currents = List.map build_node dag_node.deps in
      let deps = Current.list_seq dep_currents in
      let node =
        Day11_prep.v ~ctx:build_ctx ~hash:dag_node.hash ~deps dag_node.pkg
      in
      Hashtbl.replace node_cache dag_node.hash node;
      node
  in
  let all_build_nodes = List.map build_node nodes in
  let+ _results = Current.list_seq all_build_nodes in
  ()
