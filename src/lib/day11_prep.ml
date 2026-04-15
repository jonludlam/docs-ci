(** Day11-based build/doc nodes for the OCurrent pipeline.

    Each DAG node (build, tool, compile, link, doc-all) becomes an
    OCurrent component with its own job log, visible in the web UI.

    Uses Current_cache for job tracking but delegates all actual
    caching to day11's content-addressed layer store. *)

type t = {
  pkg : OpamPackage.t;
  build_hash : string;
  layer_dir : Fpath.t;
  all_layer_dirs : Fpath.t list;
  (** This layer's dir plus all transitive dep layer dirs.
      Used by parent builds to stack the full dep tree via overlayfs. *)
}

let pkg t = t.pkg
let build_hash t = t.build_hash
let layer_dir t = t.layer_dir
let all_layer_dirs t = t.all_layer_dirs

let has_documentable_libs t =
  Day11_doc.Doc_build.has_documentable_libs t.layer_dir

let pp f t =
  Fmt.pf f "day11-build(%s, %s)"
    (OpamPackage.to_string t.pkg)
    (String.sub t.build_hash 0 (min 12 (String.length t.build_hash)))

let compare a b = String.compare a.build_hash b.build_hash

(* ── OCurrent cache builder ──────────────────────────────────────

   A single builder handles all node types (build, tool, compile,
   link, doc-all). The [dispatch] callback determines what runs.
   Current_cache provides the job/log infrastructure; day11's disk
   cache provides the real caching. *)

module Op = struct
  type t = {
    os_dir : Fpath.t;
    dag_node : Day11_opam_layer.Build.t;
    (** The original DAG node with full deps, universe, etc. *)
    dispatch : Eio_unix.Stdenv.base -> Day11_opam_layer.Build.t -> bool;
    env : Eio_unix.Stdenv.base;
  }

  module Key = struct
    type t = {
      hash : string;
      label : string;
      pkg : OpamPackage.t;
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

  let id = "day11-node"

  let pp f (key : Key.t) =
    Fmt.pf f "%s %s" key.label (OpamPackage.to_string key.pkg)

  let auto_cancel = false

  let build (ctx : t) job (key : Key.t) =
    let open Lwt.Syntax in
    let* () = Current.Job.start job ~level:Current.Level.Average in
    let layer = Day11_layer.Layer.of_hash ~os_dir:ctx.os_dir key.hash in
    Lwt_eio.run_eio @@ fun () ->
    let cached_ok = match Day11_layer.Meta.load (Day11_layer.Layer.meta_path layer) with
      | Ok meta when meta.exit_status = 0 ->
        (* For doc layers, validate dep compile layers are present *)
        if Day11_doc.Doc_build.validate_cached_doc
             ~os_dir:ctx.os_dir (Day11_layer.Layer.dir layer)
        then true
        else begin
          Current.Job.log job "Invalidating %s: missing dep compile layers"
            key.hash;
          ignore (Bos.OS.Dir.delete ~recurse:true (Day11_layer.Layer.dir layer));
          false
        end
      | Ok _ ->
        Current.Job.log job "Clearing failed layer %s" key.hash;
        ignore (Bos.OS.Dir.delete ~recurse:true (Day11_layer.Layer.dir layer));
        false
      | Error _ -> false
    in
    if cached_ok then begin
      Current.Job.log job "Cached: %s %s" key.label
        (OpamPackage.to_string key.pkg);
      Ok Value.{
        pkg = OpamPackage.to_string key.pkg;
        hash = key.hash;
        layer_dir = Fpath.to_string (Day11_layer.Layer.dir layer);
      }
    end else begin
      Current.Job.log job "%s %s (%s)" key.label
        (OpamPackage.to_string key.pkg)
        (String.sub key.hash 0 (min 12 (String.length key.hash)));
      let success = ctx.dispatch ctx.env ctx.dag_node in
      (match Bos.OS.File.read (Day11_layer.Layer.log_path layer) with
       | Ok contents -> Current.Job.write job contents
       | Error _ -> ());
      if success then begin
        (match Day11_layer.Meta.load (Day11_layer.Layer.meta_path layer) with
         | Ok meta ->
           let tf name = Day11_layer.Meta.timing_field name meta.timing in
           Current.Job.log job "OK: %s %s (runc: %.1fs, disk: %dKB)"
             key.label (OpamPackage.to_string key.pkg)
             (tf "runc_run") (meta.disk_usage / 1024)
         | Error _ ->
           Current.Job.log job "OK: %s %s" key.label
             (OpamPackage.to_string key.pkg));
        Ok Value.{
          pkg = OpamPackage.to_string key.pkg;
          hash = key.hash;
          layer_dir = Fpath.to_string (Day11_layer.Layer.dir layer);
        }
      end else begin
        Current.Job.log job "FAILED: %s %s" key.label
          (OpamPackage.to_string key.pkg);
        Error (`Msg (Printf.sprintf "%s failed: %s" key.label
          (OpamPackage.to_string key.pkg)))
      end
    end
end

module Cache = Current_cache.Make (Op)

(* ── Public interface ──────────────────────────────────────────── *)

(** Run a DAG node as an OCurrent component with job logs.
    [dag_node] is the original DAG node with full deps and universe.
    [dispatch] is called with the original node to execute it.
    [deps] are OCurrent dependencies that must complete first. *)
let run_node ~env ~os_dir ~dispatch ~label
    ~(dag_node : Day11_opam_layer.Build.t) ~deps () : t Current.t =
  let open Current.Syntax in
  Current.component "%s %s" label (OpamPackage.to_string dag_node.pkg)
  |>
  let> deps in
  (* Collect transitive dep layer dirs from completed deps *)
  let all_dep_dirs =
    let seen = Hashtbl.create 16 in
    List.iter (fun (d : t) ->
      List.iter (fun dir ->
        let s = Fpath.to_string dir in
        if not (Hashtbl.mem seen s) then
          Hashtbl.replace seen s dir
      ) d.all_layer_dirs
    ) deps;
    Hashtbl.fold (fun _ v acc -> v :: acc) seen []
  in
  Cache.get { os_dir; dag_node; dispatch; env }
    Op.Key.{ hash = dag_node.hash; label; pkg = dag_node.pkg }
  |> Current.Primitive.map_result
       (Result.map (fun v ->
         let own_dir = Fpath.v v.Op.Value.layer_dir in
         { pkg = dag_node.pkg;
           build_hash = v.Op.Value.hash;
           layer_dir = own_dir;
           all_layer_dirs = own_dir :: all_dep_dirs }))
