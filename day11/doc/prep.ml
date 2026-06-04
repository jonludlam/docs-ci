let doc_relevant_doc_extensions =
  [ ".mld" ]

let doc_relevant_doc_files =
  [ "odoc-config.sexp" ]

(** Create prep directory structure and return bind mounts that map
    build layer lib/doc dirs into the prep layout.
    No file copying — the container sees files directly from cached layers. *)
let create_with_mounts ~source_layer_dir ~dest_layer_dir ~universe ~pkg
    ~installed_libs ~installed_docs =
  let switch = "default" in
  let pkg_name = OpamPackage.name_to_string pkg in
  let pkg_version = OpamPackage.version_to_string pkg in
  let prep_root = Fpath.(dest_layer_dir / "prep") in
  let pkg_prep =
    Fpath.(prep_root / "universes" / universe / pkg_name / pkg_version)
  in
  let lib_dest = Fpath.(pkg_prep / "lib") in
  let doc_dest = Fpath.(pkg_prep / "doc") in
  try
    Bos.OS.Dir.create ~path:true lib_dest |> ignore;
    Bos.OS.Dir.create ~path:true doc_dest |> ignore;
    let lib_src =
      Fpath.(source_layer_dir / "fs" / "home" / "opam" / ".opam"
             / switch / "lib")
    in
    (* Collect unique top-level lib subdirs to mount *)
    let lib_dirs = installed_libs |> List.filter_map (fun rel_path ->
      match String.split_on_char '/' rel_path with
      | dir :: _ -> Some dir
      | [] -> None
    ) |> List.sort_uniq String.compare in
    let lib_mounts = List.filter_map (fun dir ->
      let src = Fpath.(lib_src / dir) in
      if Bos.OS.Dir.exists src |> Result.get_ok then begin
        (* Create mount point dir in prep *)
        Bos.OS.Dir.create ~path:true Fpath.(lib_dest / dir) |> ignore;
        let container_dest = Printf.sprintf
          "/home/opam/prep/universes/%s/%s/%s/lib/%s"
          universe pkg_name pkg_version dir in
        Some (Day11_container.Mount.bind_ro
          ~src:(Fpath.to_string src) container_dest)
      end else
        None
    ) lib_dirs in
    let doc_src =
      Fpath.(source_layer_dir / "fs" / "home" / "opam" / ".opam"
             / switch / "doc")
    in
    (* For doc files: only a few .mld files, copy them directly
       (too few to justify per-file mounts) *)
    let any_mld_copied = ref false in
    List.iter (fun rel_path ->
      let ext = Filename.extension rel_path in
      let name = Filename.basename rel_path in
      if List.mem ext doc_relevant_doc_extensions
         || List.mem name doc_relevant_doc_files then begin
        let src = Fpath.(doc_src // v rel_path) in
        let dst = Fpath.(doc_dest // v rel_path) in
        Bos.OS.Dir.create ~path:true (Fpath.parent dst) |> ignore;
        if Bos.OS.File.exists src |> Result.get_ok then begin
          Bos.OS.File.read src |> Result.get_ok
          |> Bos.OS.File.write dst |> ignore;
          if ext = ".mld" then any_mld_copied := true
        end
      end
    ) installed_docs;
    (* odoc_driver_voodoo crashes ("nothing to compile") when the prep
       tree contains neither a findlib lib nor any [.mld]. For packages
       that install nothing documentable — ocaml wrappers, [conf-*]
       binding stubs, etc. — we still want voodoo to run and produce
       a real layer (so Layer.is_ok / inspect_layer agree with
       Layer_status). Drop in a one-line stub [.mld] so voodoo always
       has something to chew on. The stub is overwritten by any real
       package-supplied [.mld] (we only write it when none was copied).
       Voodoo's mld discovery walks [doc/<pkg>/], so the file goes
       there. *)
    if not !any_mld_copied && lib_mounts = [] then begin
      let stub_dir = Fpath.(doc_dest / pkg_name) in
      Bos.OS.Dir.create ~path:true stub_dir |> ignore;
      let stub = Fpath.(stub_dir / "index.mld") in
      let body = Printf.sprintf
        "{0 %s.%s}\n\nThis package installs no documentable libraries.\n"
        pkg_name pkg_version in
      Bos.OS.File.write stub body |> ignore
    end;
    Ok (prep_root, lib_mounts)
  with exn ->
    Rresult.R.error_msgf "Prep.create_with_mounts: %s"
      (Printexc.to_string exn)
