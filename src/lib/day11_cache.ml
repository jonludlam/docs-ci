(** OCurrent cache builders wrapping day11's build/doc primitives.

    Each builder uses the pre-computed layer hash as the cache digest,
    so OCurrent's caching aligns exactly with day11's content-addressed
    layer cache. If the layer exists on disk, the build is a no-op. *)


(* ── Build cache (replaces Prep) ───────────────────────────────── *)

module Build = struct
  type t = Day11_bridge.build_env

  module Key = struct
    type t = {
      pkg : OpamPackage.t;
      hash : string;
      dep_layer_dirs : Fpath.t list;
      mounts : Day11_container.Mount.t list;
    }

    let digest t = t.hash
  end

  module Value = struct
    type t = Day11_bridge.Build.t

    let marshal t =
      Printf.sprintf "%s:%s"
        (OpamPackage.to_string (Day11_bridge.Build.pkg t))
        (Day11_bridge.Build.hash t)

    let unmarshal s =
      match String.split_on_char ':' s with
      | [ pkg_str; hash ] ->
        { Day11_bridge.Build.pkg = OpamPackage.of_string pkg_str; hash }
      | _ -> failwith ("Build.Value.unmarshal: " ^ s)
  end

  let id = "day11-build"

  let pp f (key : Key.t) =
    Fmt.pf f "build %s" (OpamPackage.to_string key.pkg)

  let auto_cancel = false

  let build (ctx : t) job (key : Key.t) =
    Current.Job.log job "Building %s (hash %s)"
      (OpamPackage.to_string key.pkg)
      (String.sub key.hash 0 (min 12 (String.length key.hash)));
    (* Check if layer already exists on disk *)
    if Day11_bridge.Build.is_cached ~os_dir:ctx.os_dir
         { pkg = key.pkg; hash = key.hash } then begin
      Current.Job.log job "Layer cached on disk";
      Lwt.return_ok { Day11_bridge.Build.pkg = key.pkg; hash = key.hash }
    end else
      match Day11_bridge.Build.run ctx
              ~mounts:key.mounts ~dep_layers:key.dep_layer_dirs
              ~hash:key.hash ~pkg:key.pkg with
      | Ok build -> Lwt.return_ok build
      | Error (`Msg msg) ->
        Current.Job.log job "Build failed: %s" msg;
        Lwt.return_error (`Msg msg)
end

module BuildCache = Current_cache.Make (Build)

let build ~(ctx : Day11_bridge.build_env) ~mounts ~dep_layers ~hash ~pkg =
  let open Current.Syntax in
  Current.component "day11-build %s" (OpamPackage.to_string pkg)
  |> let> dep_layers = dep_layers
  and> mounts = mounts in
  BuildCache.get ctx
    Build.Key.{ pkg; hash; dep_layer_dirs = dep_layers; mounts }

(* ── Compile cache ─────────────────────────────────────────────── *)

module Compile = struct
  type t = Day11_bridge.doc_env

  module Key = struct
    type t = {
      pkg : OpamPackage.t;
      hash : string;
      build_layer : Fpath.t;
      dep_compile_layers : Fpath.t list;
    }

    let digest t = t.hash
  end

  module Value = struct
    type t = Day11_bridge.Compile.t

    let marshal t =
      Printf.sprintf "%s:%s"
        (OpamPackage.to_string (Day11_bridge.Compile.pkg t))
        (Day11_bridge.Compile.hash t)

    let unmarshal s =
      match String.split_on_char ':' s with
      | [ pkg_str; hash ] ->
        { Day11_bridge.Compile.pkg = OpamPackage.of_string pkg_str; hash }
      | _ -> failwith ("Compile.Value.unmarshal: " ^ s)
  end

  let id = "day11-compile"

  let pp f (key : Key.t) =
    Fmt.pf f "compile %s" (OpamPackage.to_string key.pkg)

  let auto_cancel = false

  let build (ctx : t) job (key : Key.t) =
    Current.Job.log job "Compiling docs for %s"
      (OpamPackage.to_string key.pkg);
    if Day11_bridge.Compile.is_cached ~os_dir:ctx.build.os_dir
         { pkg = key.pkg; hash = key.hash } then begin
      Current.Job.log job "Compile layer cached";
      Lwt.return_ok { Day11_bridge.Compile.pkg = key.pkg; hash = key.hash }
    end else
      match Day11_bridge.Compile.run ctx
              ~build_layer:key.build_layer
              ~dep_compile_layers:key.dep_compile_layers
              ~hash:key.hash ~pkg:key.pkg with
      | Ok compile -> Lwt.return_ok compile
      | Error (`Msg msg) ->
        Current.Job.log job "Compile failed: %s" msg;
        Lwt.return_error (`Msg msg)
end

module CompileCache = Current_cache.Make (Compile)

(* ── Link cache ────────────────────────────────────────────────── *)

module Link = struct
  type t = {
    doc_env : Day11_bridge.doc_env;
    html_dir : Fpath.t;
  }

  module Key = struct
    type t = {
      pkg : OpamPackage.t;
      hash : string;
      build_layer : Fpath.t;
      compile_layer : Fpath.t;
      dep_compile_layers : Fpath.t list;
    }

    let digest t = t.hash
  end

  module Value = Current.Unit

  let id = "day11-link"

  let pp f (key : Key.t) =
    Fmt.pf f "link %s" (OpamPackage.to_string key.pkg)

  let auto_cancel = false

  let build (ctx : t) job (key : Key.t) =
    Current.Job.log job "Linking docs for %s"
      (OpamPackage.to_string key.pkg);
    match Day11_bridge.Link.run ctx.doc_env
            ~build_layer:key.build_layer
            ~compile_layer:key.compile_layer
            ~dep_compile_layers:key.dep_compile_layers
            ~html_dir:ctx.html_dir
            ~hash:key.hash ~pkg:key.pkg with
    | Ok () ->
      Current.Job.log job "Link succeeded";
      Lwt.return_ok ()
    | Error msg ->
      Current.Job.log job "Link failed: %s" msg;
      Lwt.return_error (`Msg msg)
end

module LinkCache = Current_cache.Make (Link)

(* ── Doc-all cache ─────────────────────────────────────────────── *)

module DocAll = struct
  type t = {
    doc_env : Day11_bridge.doc_env;
    html_dir : Fpath.t;
  }

  module Key = struct
    type t = {
      pkg : OpamPackage.t;
      hash : string;
      build_layer : Fpath.t;
      dep_compile_layers : Fpath.t list;
    }

    let digest t = t.hash
  end

  module Value = struct
    type t = Day11_bridge.Doc_all.t

    let marshal t =
      Printf.sprintf "%s:%s"
        (OpamPackage.to_string (Day11_bridge.Doc_all.pkg t))
        (Day11_bridge.Doc_all.hash t)

    let unmarshal s =
      match String.split_on_char ':' s with
      | [ pkg_str; hash ] ->
        { Day11_bridge.Doc_all.pkg = OpamPackage.of_string pkg_str; hash }
      | _ -> failwith ("DocAll.Value.unmarshal: " ^ s)
  end

  let id = "day11-doc-all"

  let pp f (key : Key.t) =
    Fmt.pf f "doc-all %s" (OpamPackage.to_string key.pkg)

  let auto_cancel = false

  let build (ctx : t) job (key : Key.t) =
    Current.Job.log job "Doc-all for %s"
      (OpamPackage.to_string key.pkg);
    if Day11_bridge.Doc_all.is_cached ~os_dir:ctx.doc_env.build.os_dir
         { pkg = key.pkg; hash = key.hash } then begin
      Current.Job.log job "Doc-all layer cached";
      Lwt.return_ok { Day11_bridge.Doc_all.pkg = key.pkg; hash = key.hash }
    end else
      match Day11_bridge.Doc_all.run ctx.doc_env
              ~build_layer:key.build_layer
              ~dep_compile_layers:key.dep_compile_layers
              ~html_dir:ctx.html_dir
              ~hash:key.hash ~pkg:key.pkg with
      | Ok result -> Lwt.return_ok result
      | Error (`Msg msg) ->
        Current.Job.log job "Doc-all failed: %s" msg;
        Lwt.return_error (`Msg msg)
end

module DocAllCache = Current_cache.Make (DocAll)
