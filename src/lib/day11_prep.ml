(** Day11-based prep: build packages using layered containers.

    Replaces the OCluster + SSH prep stage. Each package is built
    locally in a container with dep layers stacked via overlayfs.

    Caching is handled entirely by day11's content-addressed layer
    store — if a layer with the right hash exists on disk, the build
    returns immediately. No OCurrent cache layer needed. *)

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

(* ── Build a single package ──────────────────────────────────────── *)

let run_build (ctx : Day11_bridge.build_env) ~dep_layer_dirs ~extra_mounts
    ~hash ~pkg =
  let os_dir = ctx.os_dir in
  let layer_dir = Day11_layer.Dir.path ~os_dir hash in
  let dep_layers = List.map Fpath.v dep_layer_dirs in
  let mounts = List.map (fun s ->
    Day11_container.Mount.bind_ro ~src:s s
  ) extra_mounts in
  (* Add opam-build binary mount if cached version exists *)
  let mounts = match Day11_opam_build.Base.opam_build_mount
    ~cache_dir:ctx.cache_dir with
    | Some m -> m :: mounts
    | None -> mounts
  in
  let node : Day11_opam_layer.Build.t = {
    hash; pkg; deps = [];
    universe = Day11_solution.Universe.dummy;
  } in
  (* If a failed layer exists on disk, delete it so it gets rebuilt.
     day11's Build_layer.build returns stale failures otherwise. *)
  let layer_json = Fpath.(layer_dir / "layer.json") in
  (match Day11_layer.Meta.load layer_json with
   | Ok meta when meta.exit_status <> 0 ->
     ignore (Bos.OS.Dir.delete ~recurse:true layer_dir)
   | _ -> ());
  (* day11's Build_layer.build handles its own disk caching:
     if layer.json exists it returns immediately *)
  let result = Day11_opam_build.Build_layer.build ctx.env ctx.benv
    ~opam_repositories:ctx.opam_repositories
    ~mounts ~build_dirs:dep_layers node () in
  (* Read build log from disk *)
  let log_file = Fpath.(layer_dir / "layer.log") in
  let log_contents = match Bos.OS.File.read log_file with
    | Ok contents -> Some contents
    | Error _ -> None
  in
  match result with
  | Day11_opam_build.Types.Success _bl ->
    Ok (layer_dir, log_contents)
  | Day11_opam_build.Types.Failure msg ->
    Error (msg, log_contents)
  | _ ->
    Error (Printf.sprintf "Build failed for %s"
      (OpamPackage.to_string pkg), log_contents)

(* ── Public interface ──────────────────────────────────────────── *)

(** Build a single package. [deps] are the already-built dep layers
    whose directories will be stacked as overlayfs lowers. *)
let v ~(ctx : Day11_bridge.build_env) ~hash ?(mounts = []) ~deps
    (pkg : OpamPackage.t) : t Current.t =
  let open Current.Syntax in
  Current.component "build %s" (OpamPackage.to_string pkg)
  |>
  let> deps in
  (* Collect ALL transitive dep layer dirs — each dep carries its own
     layer dir plus all its deps' dirs. Deduplicate. *)
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
  let dep_layer_dirs = List.map Fpath.to_string all_dep_dirs in
  let extra_mounts = List.map Fpath.to_string mounts in
  (* Run build synchronously — day11's disk cache makes this instant
     for already-built packages. No OCurrent cache layer needed. *)
  let result =
    run_build ctx ~dep_layer_dirs ~extra_mounts ~hash ~pkg
  in
  match result with
  | Ok (layer_dir, _log) ->
    Current.Primitive.const
      { pkg; build_hash = hash; layer_dir;
        all_layer_dirs = layer_dir :: all_dep_dirs }
  | Error (msg, _log) ->
    Current_incr.const (Error (`Msg msg), None)
