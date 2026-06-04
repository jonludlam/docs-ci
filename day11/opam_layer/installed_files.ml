let scan_dir ~extensions ~filenames base_dir =
  let result = ref [] in
  let rec walk prefix dir =
    try
      let dir_s = Fpath.to_string dir in
      if Sys.file_exists dir_s && Sys.is_directory dir_s then
        Sys.readdir dir_s |> Array.iter (fun name ->
          let full_path = Fpath.(dir / name) in
          let rel_path = if prefix = "" then name else prefix ^ "/" ^ name in
          try
            if Sys.is_directory (Fpath.to_string full_path) then
              walk rel_path full_path
            else if List.exists (fun ext -> Filename.check_suffix name ext) extensions
                 || List.mem name filenames then
              result := rel_path :: !result
          with Sys_error _ -> ())
    with Sys_error _ -> ()
  in
  walk "" base_dir;
  List.sort String.compare !result

let scan_libs ~layer_dir =
  let lib_dir =
    Fpath.(layer_dir / "fs" / "home" / "opam" / ".opam" / "default" / "lib")
  in
  scan_dir
    ~extensions:[ ".cmi"; ".cmti"; ".cmt"; ".cma"; ".cmxa"; ".cmx"; ".ml"; ".mli" ]
    ~filenames:[ "META"; "dune-package" ]
    lib_dir

let scan_docs ~layer_dir =
  let doc_dir =
    Fpath.(layer_dir / "fs" / "home" / "opam" / ".opam" / "default" / "doc")
  in
  scan_dir
    ~extensions:[ ".mld" ]
    ~filenames:[ "odoc-config.sexp" ]
    doc_dir
