let src = Logs.Src.create "day11.build.native_backend"
  ~doc:"Host-native opam package build backend"
module Log = (val Logs.src_log src)

module Build = Day11_opam_layer.Build
module Snapshot = Day11_layer.Snapshot

let mkdir path =
  Bos.OS.Dir.create ~path:true path |> ignore

(* -- Subprocess with custom env ------------------------------------------ *)

let run_with_env env ~cmd ~environ : Day11_sys.Run.t =
  let proc_mgr = Eio.Stdenv.process_mgr env in
  let fs = Eio.Stdenv.fs env in
  let _ = fs in
  let t_start = Unix.gettimeofday () in
  let argv =
    match cmd with
    | [] -> [ "/bin/true" ]
    | exe :: _ when String.contains exe '/' -> cmd
    | exe :: rest ->
      (* Resolve exe against PATH in environ to avoid Eio's execvp
         using the parent's PATH. *)
      let path =
        List.assoc_opt "PATH" environ
        |> Option.value ~default:"/usr/local/bin:/usr/bin:/bin"
      in
      let found =
        String.split_on_char ':' path
        |> List.find_map (fun d ->
          if d = "" then None
          else
            let cand = Filename.concat d exe in
            try Unix.access cand [ Unix.X_OK ]; Some cand
            with Unix.Unix_error _ -> None)
      in
      (match found with
       | Some p -> p :: rest
       | None -> cmd)
  in
  let env_arr =
    Array.of_list (List.map (fun (k, v) -> k ^ "=" ^ v) environ)
  in
  let run () =
    Eio.Switch.run (fun sw ->
      let r, w = Eio.Process.pipe ~sw proc_mgr in
      let child = Eio.Process.spawn ~sw proc_mgr ~env:env_arr
          ~stdout:w ~stderr:w argv in
      Eio.Flow.close w;
      let output =
        try Eio.Buf_read.(parse_exn take_all) r ~max_size:max_int
        with End_of_file -> ""
      in
      Eio.Flow.close r;
      let status = Eio.Process.await child in
      (output, status))
  in
  let output, status_eio =
    try run ()
    with Eio.Exn.Io (e, _) ->
      (Format.asprintf "native run failed: %a" Eio.Exn.pp_err e, `Exited 127)
  in
  let t_end = Unix.gettimeofday () in
  let status = match status_eio with
    | `Exited n -> `Exited n
    | `Signaled n -> `Signaled n
  in
  { Day11_sys.Run.cmd = argv;
    time = t_end -. t_start;
    output_file = None;
    output;
    errors = "";
    status }

(* -- Captured fs placement ----------------------------------------------- *)

(** Copy the files listed in [rels] (relative to [src_root]) into
    [dst_root] (keeping the same relative paths). Uses hardlinks
    when possible to avoid copying file data. *)
let place_changed_files ~src_root ~dst_root rels =
  let src_s = Fpath.to_string src_root in
  let dst_s = Fpath.to_string dst_root in
  mkdir dst_root;
  List.iter (fun rel ->
    let src = Filename.concat src_s rel in
    let dst = Filename.concat dst_s rel in
    let parent = Filename.dirname dst in
    if not (Sys.file_exists parent) then
      mkdir (Fpath.v parent);
    match Unix.lstat src with
    | exception Unix.Unix_error _ -> ()
    | st ->
      match st.st_kind with
      | Unix.S_LNK ->
        (try
          let target = Unix.readlink src in
          (try Unix.unlink dst with Unix.Unix_error _ -> ());
          Unix.symlink target dst
        with Unix.Unix_error _ -> ())
      | Unix.S_REG ->
        (try
          (try Unix.unlink dst with Unix.Unix_error _ -> ());
          Unix.link src dst
        with Unix.Unix_error _ ->
          (* Different filesystem or hardlink protection: fall back
             to copy via cp --preserve=all. *)
          let _ = Sys.command (Printf.sprintf "cp -p --no-dereference %s %s"
            (Filename.quote src) (Filename.quote dst)) in ())
      | _ -> ()
  ) rels

(* -- Entry point --------------------------------------------------------- *)

let build ~sw env (benv : Types.build_env)
    ~opam_repositories ?mounts:_
    ?patches ?build_dirs
    ?prep_upper:_ ?strategy
    (node : Build.t) ~target_fs () =
  let os_dir = benv.os_dir in
  let pkg_str = OpamPackage.to_string node.pkg in
  let strategy = match strategy with
    | Some s -> s
    | None -> Container_backend.opam_build_strategy ?patches node.pkg
  in
  let dep_dirs = match build_dirs with
    | Some dirs -> dirs
    | None ->
      Container_backend.collect_transitive_dep_dirs ~os_dir node
  in
  (* Every native build needs a valid OPAMROOT. Seed one via
     Opam_init_base and stack it AT THE BOTTOM so dep layers'
     switch-state / install records override the empty init. *)
  let dep_dirs =
    match Opam_init_base.ensure ~sw env ~os_dir
            ~opam_repositories () with
    | Ok layer ->
      Log.info (fun m -> m "Seeding native build with opam-init base %s"
        (Day11_layer.Layer.hash layer));
      dep_dirs @ [ Day11_layer.Layer.dir layer ]
    | Error (`Msg e) ->
      Log.warn (fun m -> m "Opam_init_base.ensure failed: %s" e);
      dep_dirs
  in
  let temp_dir = Bos.OS.Dir.tmp "day11_native_%s" |> Result.get_ok in
  let switch_rel = Fpath.(v "home" / "opam" / ".opam" / Types.switch) in
  let temp_home = Fpath.(temp_dir / "home" / "opam") in
  let temp_opamroot = Fpath.(temp_home / ".opam") in
  let temp_switch = Fpath.(temp_dir // switch_rel) in
  (* [DAY11_NATIVE_KEEP_TEMP=1] preserves the temp prefix under
     [$HOME/.day11/native-debug/] for post-mortem inspection. *)
  let keep_temp = Sys.getenv_opt "DAY11_NATIVE_KEEP_TEMP" <> None in
  let cleanup_temp () =
    if keep_temp then begin
      let keep_root = Fpath.(v (Sys.getenv "HOME") / ".day11"
                             / "native-debug") in
      ignore (Bos.OS.Dir.create ~path:true keep_root);
      let dst = Fpath.(keep_root / Fpath.basename temp_dir) in
      let _ = Sys.command (Printf.sprintf "mv %s %s 2>/dev/null"
        (Filename.quote (Fpath.to_string temp_dir))
        (Filename.quote (Fpath.to_string dst))) in
      Log.info (fun m -> m "DAY11_NATIVE_KEEP_TEMP — saved %a"
        Fpath.pp dst)
    end else
      ignore (Bos.OS.Path.delete ~recurse:true temp_dir)
  in
  Log.info (fun m -> m "Native build %s in %a" pkg_str Fpath.pp temp_dir);
  (* 1. Merge dep layers into temp_dir. Each dep's fs/ looks like
     fs/home/opam/.opam/default/<stuff>, so merging fs/ into temp_dir
     reconstructs that structure under temp_dir/home/opam/.opam/default/. *)
  Log.info (fun m -> m "Merging %d dep dirs into %a for %s"
    (List.length dep_dirs) Fpath.pp temp_dir pkg_str);
  List.iter (fun d ->
    Log.info (fun m -> m "  dep %a" Fpath.pp d)) dep_dirs;
  match Day11_layer.Stack.merge_no_sudo env ~layer_dirs:dep_dirs ~target:temp_dir with
  | Error (`Msg e) as err ->
    Log.err (fun m -> m "Merge failed: %s" e);
    cleanup_temp ();
    err
  | Ok () ->
    (* 2. Ensure prefix dirs exist even if no deps (first-layer builds). *)
    List.iter mkdir [ temp_home; temp_opamroot; temp_switch ];
    (* 3. Populate opam-build's repo mount location from provided repos. *)
    let repo_dst = Fpath.(temp_opamroot / "repo" / "default") in
    (match opam_repositories with
     | [] -> ()
     | repos ->
       mkdir (Fpath.parent repo_dst);
       (match Day11_opam_layer.Opam_repo.create (Fpath.parent repo_dst) with
        | Ok _ ->
          let _ = Day11_opam_layer.Opam_repo.populate ~opam_repo:repo_dst
            ~opam_repositories:repos [ node.pkg ] in ()
        | Error _ -> ()));
    (* 4. Write switch-state from merged packages dir. *)
    let packages_dir = Fpath.(temp_switch / ".opam-switch" / "packages") in
    if Bos.OS.Dir.exists packages_dir |> Result.get_ok then begin
      let state_file = Fpath.(temp_switch / ".opam-switch" / "switch-state") in
      ignore (Day11_opam_layer.Opamh.dump_state [ packages_dir ] state_file)
    end;
    (* 4b. Rewrite switch-config for the CURRENT temp path. Dep
       layers built by container carry a switch-config hard-coded
       to /home/opam/.opam, which makes opam barf with Not_found
       here. A fresh file keyed to our temp path sidesteps that. *)
    let switch_config =
      Fpath.(temp_switch / ".opam-switch" / "switch-config") in
    let user =
      try (Unix.getpwuid benv.uid).pw_name
      with Not_found -> string_of_int benv.uid
    in
    let group =
      try (Unix.getgrgid benv.gid).gr_name
      with Not_found -> string_of_int benv.gid
    in
    let _ = Bos.OS.File.write switch_config
      (Printf.sprintf
        "opam-version: \"2.0\"\n\
         synopsis: \"default\"\n\
         opam-root: %S\n\
         paths {\n}\n\
         variables {\n  user: %S\n  group: %S\n}\n"
        (Fpath.to_string temp_opamroot) user group)
    in
    (* 4c. Rewrite .opam-switch/environment. Dep layers carry a
       stale OPAM_SWITCH_PREFIX and PATH baked to the old build's
       [/tmp/day11_native_XXXXXX] path. opam-build would then
       export those and downstream tools (js_of_ocaml, dune) would
       look for findlib.conf etc. at the dead temp location. Replace
       any such embedded path with our current temp_switch. *)
    let environment_file =
      Fpath.(temp_switch / ".opam-switch" / "environment") in
    (match Bos.OS.File.read environment_file with
     | Error _ -> ()
     | Ok contents ->
       (* Scan for /tmp/day11_native_XXXXXX/home/opam/.opam/default,
          replace every occurrence with temp_switch. *)
       let needle_prefix = "/tmp/day11_native_" in
       let rec replace acc i =
         match String.index_from_opt contents i needle_prefix.[0] with
         | None -> acc ^ String.sub contents i (String.length contents - i)
         | Some j ->
           let tail_start = j + String.length needle_prefix in
           if tail_start > String.length contents
              || not (String.length contents - j >= String.length needle_prefix
                      && String.sub contents j (String.length needle_prefix) = needle_prefix)
           then replace (acc ^ String.sub contents i (j - i + 1)) (j + 1)
           else begin
             (* Advance past the 6-hex suffix and the "/home/opam/.opam/default" *)
             let rec eat_hex k =
               if k < String.length contents
                  && ((contents.[k] >= '0' && contents.[k] <= '9')
                      || (contents.[k] >= 'a' && contents.[k] <= 'f'))
               then eat_hex (k + 1) else k
             in
             let hex_end = eat_hex tail_start in
             let suffix = "/home/opam/.opam/default" in
             let suf_len = String.length suffix in
             if hex_end + suf_len <= String.length contents
                && String.sub contents hex_end suf_len = suffix
             then
               replace
                 (acc ^ String.sub contents i (j - i)
                  ^ Fpath.to_string temp_switch)
                 (hex_end + suf_len)
             else
               replace (acc ^ String.sub contents i (j - i + 1)) (j + 1)
           end
       in
       let new_contents = replace "" 0 in
       if new_contents <> contents then begin
         (* Break the hardlink from the dep layer before writing. *)
         ignore (Unix.unlink (Fpath.to_string environment_file));
         let _ = Bos.OS.File.write environment_file new_contents in
         Log.info (fun m -> m "Rewrote .opam-switch/environment prefix paths")
       end);
    (* 5. Snapshot the prefix before the build. *)
    let before = Snapshot.take env temp_switch in
    Log.info (fun m -> m "Snapshot before: %d files" (Snapshot.size before));
    (* 6. Run opam-build. Build a stripped-down env so host opam
       state doesn't bleed in (OPAM_SWITCH_PREFIX, CAML_LD_LIBRARY_PATH,
       etc. would otherwise make opam-build think the compiler is
       already installed). *)
    let opam_build_bin_dir =
      let candidates = [
        Fpath.(v (Sys.getenv "HOME") / ".day11" / "cache" / "opam-build-bin");
        Fpath.v "/usr/local/bin/opam-build";
      ] in
      List.find_map (fun p ->
        if Bos.OS.File.exists p |> Result.value ~default:false then begin
          (* If the cached binary is a single file (not directory),
             symlink it into temp_dir/bin so we have a dir to prepend
             to PATH. *)
          let fname = Fpath.basename p in
          let dir = Fpath.(temp_dir / "bin") in
          mkdir dir;
          let target = Fpath.(dir / "opam-build") in
          (try Unix.unlink (Fpath.to_string target)
           with Unix.Unix_error _ -> ());
          (try Unix.symlink (Fpath.to_string p) (Fpath.to_string target)
           with Unix.Unix_error _ -> ());
          Log.info (fun m -> m "Found opam-build at %a (as %s)"
            Fpath.pp p fname);
          Some dir
        end else None
      ) candidates
    in
    let path =
      let base =
        "/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin"
      in
      match opam_build_bin_dir with
      | Some d -> Fpath.to_string d ^ ":" ^ base
      | None -> base
    in
    let environ = [
      ("HOME", Fpath.to_string temp_home);
      ("OPAMROOT", Fpath.to_string temp_opamroot);
      ("OPAMYES", "true");
      ("OPAMNOCHECKSUMS", "false");
      ("PATH", path);
      (* Tools built in prior native builds (ocamlfind, js_of_ocaml,
         etc.) bake their build-time prefix into their binaries as the
         default [findlib.conf] lookup path. Setting OCAMLFIND_CONF
         here overrides that baked-in default so they find the
         current prefix's config instead. [findlib.conf] itself is
         relocatable ([destdir="."] etc.) so no rewriting needed. *)
      ("OCAMLFIND_CONF",
       Fpath.to_string Fpath.(temp_switch / "lib" / "findlib.conf"));
      ("OCAMLPATH",
       Fpath.to_string Fpath.(temp_switch / "lib") ^ ":" ^
       Fpath.to_string Fpath.(temp_switch / "lib" / "ocaml"));
      (* Preserve just enough of the outer env that builds need. *)
      ("LANG", try Sys.getenv "LANG" with Not_found -> "C.UTF-8");
      ("TERM", try Sys.getenv "TERM" with Not_found -> "dumb");
    ] in
    let cmd = [ "bash"; "-c"; strategy.Types.cmd ] in
    let run = run_with_env env ~cmd ~environ in
    (* 7. Apply strategy.cleanup so transient files aren't captured.
       The cleanup paths use [upper/home/opam/.opam/...] — temp_dir
       plays the role of [upper] in native mode. *)
    strategy.cleanup ~sw env temp_dir;
    (* 8. Diff: list files new or modified under temp_switch. *)
    let changed = Snapshot.diff env ~before temp_switch in
    Log.info (fun m -> m "Build %s: %d changed files" pkg_str
      (List.length changed));
    (* 9. Place changed files into target_fs under the same
       home/opam/.opam/default/<rel> layout that container layers use. *)
    let target_switch = Fpath.(target_fs // switch_rel) in
    place_changed_files ~src_root:temp_switch ~dst_root:target_switch changed;
    mkdir target_fs;
    let timing = [ "total", run.time ] in
    cleanup_temp ();
    Ok (run, timing)
