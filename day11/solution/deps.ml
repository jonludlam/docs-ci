type t = OpamPackage.Set.t OpamPackage.Map.t

(* Memoized recursive transitive closure. Cycle-safe: re-entering an
   in-progress pkg returns the empty set rather than recursing
   forever, so a cycle [a → b → a] yields a closure that includes
   the back-edge target via [direct] but doesn't keep expanding.
   Callers that care about cycles still want [has_cycle] up-front
   — but pipelines that pass doc-deps in here (where back-edges are
   possible via [{post}]-guarded entries) need the safety net.
   Without it, [transitive_deps doc_deps] stack-overflows in the
   wild. *)
let transitive_deps deps =
  let cache = Hashtbl.create 64 in
  let visiting = Hashtbl.create 16 in
  let rec go pkg =
    match Hashtbl.find_opt cache pkg with
    | Some d -> d
    | None ->
      if Hashtbl.mem visiting pkg then
        OpamPackage.Set.empty
      else begin
        Hashtbl.add visiting pkg ();
        let direct = match OpamPackage.Map.find_opt pkg deps with
          | Some s -> s | None -> OpamPackage.Set.empty in
        let transitive =
          OpamPackage.Set.fold (fun dep acc ->
            OpamPackage.Set.union acc (go dep)
          ) direct direct
        in
        Hashtbl.remove visiting pkg;
        Hashtbl.replace cache pkg transitive;
        transitive
      end
  in
  OpamPackage.Map.mapi (fun pkg _ -> go pkg) deps

(* DFS with a three-colour marker: white (not seen), grey (on stack),
   black (done). A grey re-entry means we found a back-edge — i.e. a
   cycle. Stops at the first cycle. *)
let has_cycle deps =
  let colour : (OpamPackage.t, [ `Grey | `Black ]) Hashtbl.t =
    Hashtbl.create 64 in
  let exception Cycle in
  let rec visit pkg =
    match Hashtbl.find_opt colour pkg with
    | Some `Black -> ()
    | Some `Grey -> raise Cycle
    | None ->
      Hashtbl.add colour pkg `Grey;
      (match OpamPackage.Map.find_opt pkg deps with
       | Some direct -> OpamPackage.Set.iter visit direct
       | None -> ());
      Hashtbl.replace colour pkg `Black
  in
  try
    OpamPackage.Map.iter (fun pkg _ -> visit pkg) deps;
    false
  with Cycle -> true

let compiler_names = [ "ocaml-base-compiler"; "ocaml-variants"; "ocaml" ]

let extract_ocaml_version deps =
  List.find_map (fun name ->
    let name = OpamPackage.Name.of_string name in
    OpamPackage.Map.filter (fun pkg _ ->
      OpamPackage.Name.equal (OpamPackage.name pkg) name
    ) deps
    |> OpamPackage.Map.min_binding_opt
    |> Option.map fst
  ) compiler_names
