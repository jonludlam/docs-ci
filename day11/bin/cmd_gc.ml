(** gc command: reclaim disk space from the shared cache *)

open Cmdliner

let du cmd =
  try
    let ic = Unix.open_process_in cmd in
    let line = input_line ic in
    ignore (Unix.close_process_in ic);
    String.trim line
  with _ -> "?"

let run profile_dir before_days delete =
  Common.with_eio @@ fun ~sw:_ env ->
  let pdir = Common.resolve_profile_dir profile_dir in
  let cache_dir = Fpath.(pdir / "cache") in
  Printf.printf "=== Garbage Collection ===\n\n";
  Printf.printf "Cache: %s\n" (Fpath.to_string cache_dir);
  Printf.printf "  Size: %s\n\n"
    (du (Printf.sprintf "du -sh %s 2>/dev/null | cut -f1"
      (Fpath.to_string cache_dir)));
  (* 1. Clean stale temp dirs *)
  let n_stale = Day11_lib.Gc.gc_stale_temp_dirs () in
  if n_stale > 0 then
    Printf.printf "Cleaned %d stale overlay temp dirs\n\n" n_stale;
  (* 2. Scan layers by last-used time *)
  let cutoff = Unix.gettimeofday () -. (float before_days *. 86400.) in
  let os_dirs =
    match Bos.OS.Dir.contents cache_dir with
    | Error _ -> []
    | Ok entries ->
      List.filter (fun p ->
        let name = Fpath.basename p in
        name <> "base" && name <> "opam-build-bin" &&
        (Bos.OS.Dir.exists p |> Result.get_ok)
      ) entries
  in
  let total_layers = ref 0 in
  let old_layers = ref 0 in
  let old_size = ref 0 in
  let deleted_count = ref 0 in
  List.iter (fun os_dir ->
    let os_name = Fpath.basename os_dir in
    let entries =
      try Sys.readdir (Fpath.to_string os_dir) |> Array.to_list
      with _ -> [] in
    let layers = List.filter (fun name ->
      (* Layer dirs: 12-char hex or build-<hex> (legacy) *)
      let is_hex c = (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') in
      (String.length name = 12 && String.for_all is_hex name)
      || (String.length name > 6 && String.sub name 0 6 = "build-")
    ) entries in
    total_layers := !total_layers + List.length layers;
    List.iter (fun name ->
      let layer_dir = Fpath.(os_dir / name) in
      let last_used = match Day11_layer.Last_used.get env layer_dir with
        | Some t -> t | None -> 0.0 in
      if last_used < cutoff then begin
        incr old_layers;
        let size =
          try (Unix.stat (Fpath.to_string layer_dir)).Unix.st_size
          with _ -> 0 in
        old_size := !old_size + size;
        if delete then begin
          ignore (Sys.command (Printf.sprintf "sudo rm -rf %s"
            (Fpath.to_string layer_dir)));
          incr deleted_count
        end
      end
    ) layers;
    Printf.printf "  %s: %d layers\n" os_name (List.length layers)
  ) os_dirs;
  Printf.printf "\nTotal layers: %d\n" !total_layers;
  Printf.printf "Layers last used before %d days ago: %d\n"
    before_days !old_layers;
  if delete then
    Printf.printf "Deleted: %d layers\n" !deleted_count
  else if !old_layers > 0 then
    Printf.printf "Run with --delete to remove them.\n";
  Printf.printf "\nCache after: %s\n"
    (du (Printf.sprintf "du -sh %s 2>/dev/null | cut -f1"
      (Fpath.to_string cache_dir)));
  0

let before_term =
  let doc = "Delete layers not used in the last N days (default 30)" in
  Arg.(value & opt int 30 & info [ "before" ] ~docv:"DAYS" ~doc)

let delete_term =
  let doc = "Actually delete old layers (default: report only)" in
  Arg.(value & flag & info [ "delete" ] ~doc)

let cmd =
  let info = Cmd.info "gc"
    ~doc:"Reclaim disk space by removing old layers from the shared cache" in
  let term = Term.(const run $ Common.profile_dir_term
    $ before_term $ delete_term) in
  Cmd.v info term
