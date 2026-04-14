(** Day11-based doc compile/link: generate docs using layered containers.

    Replaces the OCluster-based compile stage. Uses day11's Doc_build
    primitives with overlayfs stacking of compile layers. *)

type jobty = CompileAndLink | CompileOnly | LinkOnly

type t = {
  package : Package.t;
  compile_hash : string;
  compile_layer_dir : Fpath.t option;  (* None for LinkOnly *)
}

let package t = t.package
let compile_hash t = t.compile_hash

(* ── OCurrent cache builder ────────────────────────────────────── *)

module CompileOp = struct
  type t = {
    doc_env : Day11_bridge.doc_env;
    html_dir : Fpath.t;
  }

  module Key = struct
    type t = {
      pkg : OpamPackage.t;
      hash : string;
      build_layer_dir : string;
      dep_compile_layer_dirs : string list;
      jobty : jobty;
    }

    let digest t = t.hash
  end

  module Value = struct
    type t = {
      pkg : string;
      hash : string;
      compile_layer_dir : string option;
    }
    [@@deriving yojson]

    let marshal t = Yojson.Safe.to_string (to_yojson t)
    let unmarshal s = of_yojson (Yojson.Safe.from_string s) |> Result.get_ok
  end

  let id = "day11-doc"

  let pp f (key : Key.t) =
    let phase = match key.jobty with
      | CompileAndLink -> "doc-all"
      | CompileOnly -> "compile"
      | LinkOnly -> "link"
    in
    Fmt.pf f "%s %s" phase (OpamPackage.to_string key.pkg)

  let auto_cancel = false

  let build (ctx : t) job (key : Key.t) =
    let phase_str = match key.jobty with
      | CompileAndLink -> "doc-all"
      | CompileOnly -> "compile"
      | LinkOnly -> "link"
    in
    Current.Job.log job "%s %s" phase_str (OpamPackage.to_string key.pkg);
    let build_layer = Fpath.v key.build_layer_dir in
    let dep_compile_layers = List.map Fpath.v key.dep_compile_layer_dirs in
    let os_dir = ctx.doc_env.build.os_dir in
    match key.jobty with
    | CompileOnly ->
      (match Day11_bridge.Compile.run ctx.doc_env
               ~build_layer ~dep_compile_layers
               ~hash:key.hash ~pkg:key.pkg with
       | Ok result ->
         let dir = Day11_bridge.Compile.layer_dir ~os_dir result in
         Lwt.return_ok Value.{
           pkg = OpamPackage.to_string key.pkg;
           hash = key.hash;
           compile_layer_dir = Some (Fpath.to_string dir);
         }
       | Error (`Msg msg) ->
         Lwt.return_error (`Msg msg))
    | LinkOnly ->
      let compile_layer = Fpath.v key.build_layer_dir in (* reuse field *)
      (match Day11_bridge.Link.run ctx.doc_env
               ~build_layer ~compile_layer
               ~dep_compile_layers ~html_dir:ctx.html_dir
               ~hash:key.hash ~pkg:key.pkg with
       | Ok () ->
         Lwt.return_ok Value.{
           pkg = OpamPackage.to_string key.pkg;
           hash = key.hash;
           compile_layer_dir = None;
         }
       | Error msg ->
         Lwt.return_error (`Msg msg))
    | CompileAndLink ->
      (match Day11_bridge.Doc_all.run ctx.doc_env
               ~build_layer ~dep_compile_layers
               ~html_dir:ctx.html_dir
               ~hash:key.hash ~pkg:key.pkg with
       | Ok result ->
         let dir = Day11_bridge.Doc_all.layer_dir ~os_dir result in
         Lwt.return_ok Value.{
           pkg = OpamPackage.to_string key.pkg;
           hash = key.hash;
           compile_layer_dir = Some (Fpath.to_string dir);
         }
       | Error (`Msg msg) ->
         Lwt.return_error (`Msg msg))
end

module Cache = Current_cache.Make (CompileOp)

(** Run a doc compile/link/doc-all job for a package. *)
let v ~(ctx : CompileOp.t) ~hash ~jobty ~build_layer_dir
    ~deps (package : Package.t) : t Current.t =
  let open Current.Syntax in
  let phase = match jobty with
    | CompileAndLink -> "doc-all"
    | CompileOnly -> "compile"
    | LinkOnly -> "link"
  in
  Current.component "day11-%s %s" phase
    (OpamPackage.to_string (Package.opam package))
  |>
  let> deps in
  let dep_compile_layer_dirs = List.filter_map (fun (d : t) ->
    Option.map Fpath.to_string d.compile_layer_dir) deps in
  let pkg = Package.opam package in
  Cache.get ctx
    CompileOp.Key.{
      pkg; hash; jobty;
      build_layer_dir = Fpath.to_string build_layer_dir;
      dep_compile_layer_dirs;
    }
  |> Current.Primitive.map_result
       (Result.map (fun v ->
         { package;
           compile_hash = v.CompileOp.Value.hash;
           compile_layer_dir = Option.map Fpath.v v.compile_layer_dir }))
