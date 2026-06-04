type t = {
  hash : string;
  dir : Fpath.t;
}

let create ~base_dir hash =
  let dir = Fpath.(base_dir / ("epoch-" ^ hash)) in
  Bos.OS.Dir.create ~path:true dir |> ignore;
  { hash; dir }

let promote ~base_dir t =
  let link = Fpath.(base_dir / "html-live") in
  let link_s = Fpath.to_string link in
  let target = Fpath.to_string Fpath.(t.dir / "html") in
  (try Unix.unlink link_s with Unix.Unix_error _ -> ());
  Unix.symlink target link_s

let current ~base_dir =
  let link = Fpath.(base_dir / "html-live") in
  let link_s = Fpath.to_string link in
  try
    let target = Unix.readlink link_s in
    let parent = Filename.dirname target in
    let name = Filename.basename parent in
    if String.length name > 6 && String.sub name 0 6 = "epoch-" then
      let hash = String.sub name 6 (String.length name - 6) in
      Some { hash; dir = Fpath.v parent }
    else None
  with Unix.Unix_error _ -> None

let gc ~base_dir ~keep =
  let base_s = Fpath.to_string base_dir in
  let entries =
    try Sys.readdir base_s |> Array.to_list
    with Sys_error _ -> []
  in
  let epochs = List.filter_map (fun name ->
    if String.length name > 6 && String.sub name 0 6 = "epoch-" then
      let dir = Fpath.(base_dir / name) in
      let mtime =
        try (Unix.stat (Fpath.to_string dir)).Unix.st_mtime
        with _ -> 0.0
      in
      Some (name, dir, mtime)
    else None
  ) entries in
  let sorted = List.sort (fun (_, _, a) (_, _, b) -> compare b a) epochs in
  let to_delete = if List.length sorted > keep then
    List.filteri (fun i _ -> i >= keep) sorted
  else [] in
  List.iter (fun (_, dir, _) ->
    Bos.OS.Dir.delete ~recurse:true dir |> ignore
  ) to_delete;
  List.length to_delete
