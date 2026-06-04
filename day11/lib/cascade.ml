type status =
  | Ok
  | Failed
  | Cascade of string
  | Pending

type result = {
  status : status;
  pkg : OpamPackage.t;
  kind : Dag_marshal.kind;
}

(** Build [pkg → (build_hash → entry)] from disk, scanning each unique
    package once. Latest entry per hash wins (matches
    {!History.read_latest}). *)
let load_history_index ~packages_dir entries =
  let by_pkg : (string, (string, History.entry) Hashtbl.t) Hashtbl.t =
    Hashtbl.create 64 in
  let load_pkg pkg_str =
    if Hashtbl.mem by_pkg pkg_str then ()
    else begin
      let entries = History.read_latest ~packages_dir ~pkg_str in
      let tbl = Hashtbl.create (List.length entries) in
      List.iter (fun (h : History.entry) ->
        if not (Hashtbl.mem tbl h.build_hash) then
          Hashtbl.add tbl h.build_hash h
      ) entries;
      Hashtbl.add by_pkg pkg_str tbl
    end
  in
  List.iter (fun (e : Dag_marshal.entry) ->
    load_pkg (OpamPackage.to_string e.pkg)
  ) entries;
  by_pkg

let lookup_history index pkg hash =
  let pkg_str = OpamPackage.to_string pkg in
  match Hashtbl.find_opt index pkg_str with
  | None -> None
  | Some tbl -> Hashtbl.find_opt tbl hash

let classify ~packages_dir entries =
  let by_hash : (string, Dag_marshal.entry) Hashtbl.t =
    Hashtbl.create (List.length entries) in
  List.iter (fun (e : Dag_marshal.entry) ->
    Hashtbl.replace by_hash e.hash e) entries;
  let history_index = load_history_index ~packages_dir entries in
  let table : (string, result) Hashtbl.t =
    Hashtbl.create (List.length entries) in
  let visiting : (string, unit) Hashtbl.t = Hashtbl.create 64 in
  let rec classify_one h =
    match Hashtbl.find_opt table h with
    | Some r -> Some r
    | None ->
      match Hashtbl.find_opt by_hash h with
      | None -> None
      | Some e ->
        if Hashtbl.mem visiting h then begin
          (* DAG cycle — shouldn't happen, but stay safe. *)
          let r = { status = Pending; pkg = e.pkg; kind = e.kind } in
          Hashtbl.replace table h r;
          Some r
        end else begin
          Hashtbl.add visiting h ();
          List.iter (fun dh ->
            ignore (classify_one dh)
          ) e.deps;
          Hashtbl.remove visiting h;
          let status =
            match lookup_history history_index e.pkg h with
            | Some he when he.status = "success" -> Ok
            | Some he when he.status = "failure" -> Failed
            | Some _ | None ->
              (* No matching history entry — node didn't run.
                 Look for a failed/cascaded dep to pin attribution to. *)
              let cascade_source =
                List.find_map (fun dh ->
                  match Hashtbl.find_opt table dh with
                  | Some { status = Failed; _ } -> Some dh
                  | Some { status = Cascade src; _ } -> Some src
                  | _ -> None
                ) e.deps
              in
              (match cascade_source with
               | Some src -> Cascade src
               | None -> Pending)
          in
          let r = { status; pkg = e.pkg; kind = e.kind } in
          Hashtbl.replace table h r;
          Some r
        end
  in
  List.iter (fun (e : Dag_marshal.entry) ->
    ignore (classify_one e.hash)
  ) entries;
  table

let root_cause table hash =
  match Hashtbl.find_opt table hash with
  | Some { status = Failed; _ } -> Some hash
  | Some { status = Cascade src; _ } -> Some src
  | _ -> None

let classify_from_layer_index ~status_index entries =
  let layer_status hash =
    let len = min 12 (String.length hash) in
    let key = String.sub hash 0 len in
    match Hashtbl.find_opt status_index key with
    | None -> `Missing
    | Some (e : Day11_layer.Layer_status.entry) ->
      if e.exit_status = 0 then `Ok else `Failed
  in
  let by_hash : (string, Dag_marshal.entry) Hashtbl.t =
    Hashtbl.create (List.length entries) in
  List.iter (fun (e : Dag_marshal.entry) ->
    Hashtbl.replace by_hash e.hash e) entries;
  let table : (string, result) Hashtbl.t =
    Hashtbl.create (List.length entries) in
  let visiting : (string, unit) Hashtbl.t = Hashtbl.create 64 in
  let rec classify_one h =
    match Hashtbl.find_opt table h with
    | Some r -> Some r
    | None ->
      match Hashtbl.find_opt by_hash h with
      | None -> None
      | Some e ->
        if Hashtbl.mem visiting h then begin
          let r = { status = Pending; pkg = e.pkg; kind = e.kind } in
          Hashtbl.replace table h r; Some r
        end else begin
          Hashtbl.add visiting h ();
          List.iter (fun dh -> ignore (classify_one dh)) e.deps;
          Hashtbl.remove visiting h;
          let status = match layer_status h with
            | `Ok -> Ok
            | `Failed -> Failed
            | `Missing ->
              let cascade_source =
                List.find_map (fun dh ->
                  match Hashtbl.find_opt table dh with
                  | Some { status = Failed; _ } -> Some dh
                  | Some { status = Cascade src; _ } -> Some src
                  | _ -> None
                ) e.deps
              in
              (match cascade_source with
               | Some src -> Cascade src
               | None -> Pending)
          in
          let r = { status; pkg = e.pkg; kind = e.kind } in
          Hashtbl.replace table h r;
          Some r
        end
  in
  List.iter (fun (e : Dag_marshal.entry) ->
    ignore (classify_one e.hash)
  ) entries;
  table

let classify_from_layers ~os_dir entries =
  let status_index = Day11_layer.Layer_status.load ~os_dir in
  classify_from_layer_index ~status_index entries
