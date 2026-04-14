(** Day11-based prep: build packages using layered containers.

    Replaces the OCluster + SSH prep stage. Each package is built
    locally in a container with dep layers stacked via overlayfs.

    This module provides an OCurrent-compatible [v] function with the
    same shape as [Prep.v] — it takes deps as [t list Current.t] and
    returns [t Current.t], letting OCurrent handle DAG scheduling. *)

type t = {
  package : Package.t;
  build_hash : string;
  layer_dir : Fpath.t;
}

let package t = t.package
let build_hash t = t.build_hash
let layer_dir t = t.layer_dir

let has_documentable_libs t =
  Day11_doc.Doc_build.has_documentable_libs t.layer_dir

let pp f t =
  Fmt.pf f "day11-build(%s, %s)"
    (OpamPackage.to_string (Package.opam t.package))
    (String.sub t.build_hash 0 (min 12 (String.length t.build_hash)))

let compare a b = String.compare a.build_hash b.build_hash

(* ── OCurrent cache builder ────────────────────────────────────── *)

module BuildOp = struct
  type t = Day11_bridge.build_env

  module Key = struct
    type t = {
      pkg : OpamPackage.t;
      hash : string;
      dep_layer_dirs : string list;  (* Fpath.t not serializable *)
    }

    let digest t = t.hash
  end

  module Value = struct
    type t = {
      pkg : string;
      hash : string;
      layer_dir : string;
    }
    [@@deriving yojson]

    let marshal t = Yojson.Safe.to_string (to_yojson t)
    let unmarshal s = of_yojson (Yojson.Safe.from_string s) |> Result.get_ok
  end

  let id = "day11-build"

  let pp f (key : Key.t) =
    Fmt.pf f "build %s" (OpamPackage.to_string key.pkg)

  let auto_cancel = false

  let build (ctx : t) job (key : Key.t) =
    Current.Job.log job "Building %s (hash: %s)"
      (OpamPackage.to_string key.pkg)
      (String.sub key.hash 0 (min 12 (String.length key.hash)));
    let os_dir = ctx.os_dir in
    let layer_dir = Day11_layer.Dir.path ~os_dir key.hash in
    (* Check disk cache *)
    if Bos.OS.File.exists Fpath.(layer_dir / "layer.json") |> Result.get_ok then begin
      Current.Job.log job "Cached on disk";
      Lwt.return_ok Value.{
        pkg = OpamPackage.to_string key.pkg;
        hash = key.hash;
        layer_dir = Fpath.to_string layer_dir;
      }
    end else begin
      let dep_layers = List.map Fpath.v key.dep_layer_dirs in
      let node : Day11_opam_layer.Build.t = {
        hash = key.hash; pkg = key.pkg; deps = [];
        universe = Day11_solution.Universe.dummy;
      } in
      let result = Day11_opam_build.Build_layer.build ctx.env ctx.benv
        ~mounts:[] ~build_dirs:dep_layers node () in
      match result with
      | Day11_opam_build.Types.Success _bl ->
        Current.Job.log job "Build succeeded";
        Lwt.return_ok Value.{
          pkg = OpamPackage.to_string key.pkg;
          hash = key.hash;
          layer_dir = Fpath.to_string layer_dir;
        }
      | _ ->
        Lwt.return_error (`Msg (Printf.sprintf "Build failed for %s"
          (OpamPackage.to_string key.pkg)))
    end
end

module Cache = Current_cache.Make (BuildOp)

(* ── Public interface ──────────────────────────────────────────── *)

(** Build a single package. [deps] are the already-built dep layers
    whose directories will be stacked as overlayfs lowers. *)
let v ~(ctx : Day11_bridge.build_env) ~hash ~deps
    (package : Package.t) : t Current.t =
  let open Current.Syntax in
  Current.component "day11-build %s"
    (OpamPackage.to_string (Package.opam package))
  |>
  let> deps in
  let dep_layer_dirs = List.map (fun (d : t) ->
    Fpath.to_string d.layer_dir) deps in
  let pkg = Package.opam package in
  Cache.get ctx
    BuildOp.Key.{ pkg; hash; dep_layer_dirs }
  |> Current.Primitive.map_result
       (Result.map (fun v ->
         { package;
           build_hash = v.BuildOp.Value.hash;
           layer_dir = Fpath.v v.layer_dir }))
