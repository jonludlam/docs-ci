module Build = Day11_opam_layer.Build
module Tool = Day11_opam_layer.Tool
type build = Build.t

let build_dag cache ~base_hash solutions =
  (* Memo by build hash. The hash is the only true identity of a
     build: two solutions can reach [pkg] with the same build-deps
     closure but different doc-deps closures (different universes) —
     they produce identical layer content, and the downstream
     [build_by_hash] / cache lookup already collapses them. Memoing
     by [(pkg, universe)] used to produce one [Build.t] record per
     universe and dump them all into the dag.json list, leaving the
     serialised file with up to 57× identical entries per node
     (csexp.1.5.2 tool, dune-configurator.3.23.1 tool, etc.). Keyed
     by hash, the memo carries one node per actual build, and the
     [universe] field stored on the node is just the first-touching
     solution's view — that's already the case post-dedup downstream,
     so no information is lost. *)
  let memo : (string, build) Hashtbl.t = Hashtbl.create 256 in
  let rec get_node solution trans_build trans_doc pkg =
    let pkg_build_deps =
      match OpamPackage.Map.find_opt pkg trans_build with
      | Some s -> OpamPackage.Set.elements s
      | None -> []
    in
    let all_pkgs = pkg :: pkg_build_deps in
    let hash = Hash_cache.layer_hash cache ~base_hash all_pkgs in
    match Hashtbl.find_opt memo hash with
    | Some node -> node
    | None ->
      (* Universe identity reflects the {b doc-deps} closure. Two
         solutions sharing build-deps but differing in doc-deps will
         hash-collide here (same [hash]); the first-arriving one's
         universe is the one we keep — same convention as
         [build_by_hash]'s last-write-wins. Falls back to build-deps
         when [pkg] isn't in [trans_doc] (defensive — shouldn't
         happen, since doc_solution ⊇ solution). *)
      let pkg_universe_deps =
        match OpamPackage.Map.find_opt pkg trans_doc with
        | Some s -> OpamPackage.Set.elements s
        | None -> pkg_build_deps
      in
      let universe =
        Day11_solution.Universe.of_deps
          (OpamPackage.Set.of_list pkg_universe_deps) in
      let direct_deps =
        match OpamPackage.Map.find_opt pkg solution with
        | Some s -> OpamPackage.Set.elements s
        | None -> []
      in
      let deps = List.filter_map (fun dep ->
        if OpamPackage.Map.mem dep solution then
          Some (get_node solution trans_build trans_doc dep)
        else
          None
      ) direct_deps in
      let node : build = { hash; pkg; deps; universe } in
      Hashtbl.replace memo hash node;
      node
  in
  List.iter (fun (_target, solution, doc_solution) ->
    let trans_build = Day11_solution.Deps.transitive_deps solution in
    let trans_doc = Day11_solution.Deps.transitive_deps doc_solution in
    OpamPackage.Map.iter (fun pkg _deps ->
      ignore (get_node solution trans_build trans_doc pkg)
    ) solution
  ) solutions;
  let all_nodes = Hashtbl.fold (fun _ node acc -> node :: acc) memo [] in
  List.sort (fun (a : build) (b : build) ->
    compare (List.length a.deps) (List.length b.deps)) all_nodes
