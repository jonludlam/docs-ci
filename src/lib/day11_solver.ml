(** Day11-based solver: in-process solving via solver_pool subprocesses.

    Replaces the Cap'n Proto solver service. Uses day11's solver_worker
    binaries for parallel solving, communicating via JSONL files. *)

module SolveOp = struct
  type t = {
    repos_with_shas : (string * string) list;
    env : Eio_unix.Stdenv.base;
    np : int;
    profile_name : string;
    ocaml_version : OpamPackage.t option;
    cache_dir : Fpath.t;
    (** Used to derive the per-snapshot [solutions/] directory
        ([snapshot_dir/solutions/<pkg>.json]). The on-disk cache lets
        a pipeline restart after [sqlite.db] has been wiped (or after
        OCurrent's primitive cache otherwise loses state) skip the
        ~3-minute solver pass entirely as long as repos haven't moved
        and the compiler / target set is unchanged. *)
  }

  module Key = struct
    type t = {
      targets : OpamPackage.t list;
      commit : string;
      repos_digest : string;
      ocaml_version : string;
      (* String form — empty means unpinned. Including this in the
         cache key ensures a profile compiler change invalidates
         prior solves. *)
    }

    let digest t =
      t.commit ^ "@" ^ t.repos_digest ^ "|" ^ t.ocaml_version ^ ":" ^
      (List.map OpamPackage.to_string t.targets
       |> String.concat ",")
  end

  module Value = struct
    type t = {
      results : (string * string) list;
      (* (pkg_str, solve_result_json) pairs *)
    }
    [@@deriving yojson]

    let marshal t = Yojson.Safe.to_string (to_yojson t)
    let unmarshal s = of_yojson (Yojson.Safe.from_string s) |> Result.get_ok
  end

  let id = "day11-solver"

  let pp f (key : Key.t) =
    Fmt.pf f "solve %d packages @%s"
      (List.length key.targets)
      (String.sub key.commit 0 (min 12 (String.length key.commit)))

  let auto_cancel = false

  let snapshot_solutions_dir ctx =
    let snapshot_dir =
      Day11_profile_ctx_loader.snapshot_dir_of
        ~cache_dir:ctx.cache_dir
        ~profile_name:ctx.profile_name
        ctx.repos_with_shas
    in
    Fpath.(snapshot_dir / "solutions")

  (* Per-target solution cache.

     Each [<pkg>.json] file wraps the solver output in an envelope
     that embeds a [cache_key] = hash(compiler ∥ commit ∥
     repos_digest). At load time we drop any file whose embedded key
     doesn't match the current profile state, so a repo commit or
     compiler change invalidates just those files — other cached
     solutions stay valid.

     This is deliberately per-file (not a directory-level
     fingerprint) so adding a new target to opam-repo doesn't nuke
     the cache for every existing target. Failed solves are simply
     absent on disk; they get re-attempted on the next run, which
     is usually what we want. *)
  let solution_filename pkg =
    OpamPackage.to_string pkg ^ ".json"

  let compute_cache_key ~compiler_tag ~commit ~repos_digest =
    Digest.to_hex (Digest.string
      (compiler_tag ^ "|" ^ commit ^ "|" ^ repos_digest))

  let wrap_solution ~cache_key result_json =
    Yojson.Safe.to_string (`Assoc [
      "cache_key", `String cache_key;
      "result", result_json;
    ])

  (* Returns [Some result_json_string] if the on-disk file is an
     envelope whose embedded [cache_key] matches. Any parse error,
     missing key, or key mismatch returns [None] — caller treats
     that as a cache miss for this target. *)
  let unwrap_solution ~cache_key contents =
    match Yojson.Safe.from_string contents with
    | `Assoc fields ->
      (match List.assoc_opt "cache_key" fields,
             List.assoc_opt "result" fields with
       | Some (`String k), Some result when k = cache_key ->
         Some (Yojson.Safe.to_string result)
       | _ -> None)
    | _ -> None
    | exception _ -> None

  (* Split [targets] into those whose cached solutions are still
     valid (matching [cache_key]) and those that need (re)solving. *)
  let partition_cached ~dir ~cache_key targets =
    if not (Bos.OS.Dir.exists dir |> Result.value ~default:false)
    then ([], targets)
    else
      List.fold_left (fun (cached, uncached) pkg ->
        let path = Fpath.(dir / solution_filename pkg) in
        match Bos.OS.File.read path with
        | Ok contents ->
          (match unwrap_solution ~cache_key contents with
           | Some result_json ->
             (OpamPackage.to_string pkg, result_json) :: cached, uncached
           | None -> cached, pkg :: uncached)
        | Error _ -> cached, pkg :: uncached
      ) ([], []) targets

  let save_result ~dir ~cache_key pkg result_json =
    let path = Fpath.(dir / solution_filename pkg) in
    ignore (Bos.OS.File.write path (wrap_solution ~cache_key result_json))

  let build (ctx : t) job (key : Key.t) =
    let open Lwt.Syntax in
    let* () = Current.Job.start job ~level:Current.Level.Mostly_harmless in
    let compiler_tag =
      if key.ocaml_version = "" then "none" else key.ocaml_version in
    let cache_key =
      compute_cache_key ~compiler_tag
        ~commit:key.commit ~repos_digest:key.repos_digest in
    let dir = snapshot_solutions_dir ctx in
    ignore (Bos.OS.Dir.create ~path:true dir);
    Lwt_eio.run_eio @@ fun () ->
    let cached, uncached =
      partition_cached ~dir ~cache_key key.targets in
    let n_cached = List.length cached in
    let n_uncached = List.length uncached in
    let short_commit =
      String.sub key.commit 0 (min 12 (String.length key.commit)) in
    if n_uncached = 0 then begin
      Current.Job.log job
        "[profile %s] All %d solutions cached (commit %s) — skipping solver"
        ctx.profile_name n_cached short_commit;
      Ok Value.{ results = cached }
    end else begin
      Current.Job.log job
        "[profile %s] %d cached, solving %d new/stale targets (commit %s)"
        ctx.profile_name n_cached n_uncached short_commit;
      Eio.Switch.run @@ fun sw ->
      let results =
        Day11_solver_pool.Solver_pool.solve_many ~sw ctx.env
          ?ocaml_version:ctx.ocaml_version
          ~on_progress:(fun ~done_count ~total ->
            Current.Job.log job "Solving: %d/%d" done_count total)
          ~np:ctx.np ~repos:ctx.repos_with_shas uncached
      in
      let new_pairs = List.filter_map (fun (pkg, result) ->
        match result with
        | Ok solve_result ->
          let result_json = Day11_solution.Solve_result.to_json solve_result in
          save_result ~dir ~cache_key pkg result_json;
          Some (OpamPackage.to_string pkg, Yojson.Safe.to_string result_json)
        | Error _ -> None
      ) results in
      Current.Job.log job
        "Solved %d/%d new targets; %d cached → %d total solutions"
        (List.length new_pairs) n_uncached n_cached
        (List.length new_pairs + n_cached);
      Ok Value.{ results = cached @ new_pairs }
    end
end

module Solver_cache = Current_cache.Make (SolveOp)

type solution = {
  target : OpamPackage.t;
  solve_result : Day11_solution.Solve_result.t;
}

(** Solve all tracked packages using day11's solver. Returns solutions
    keyed by target package. *)
let repos_digest repos =
  let sorted = List.sort compare
    (List.map (fun (path, sha) -> path ^ "@" ^ sha) repos) in
  Digest.to_hex (Digest.string (String.concat "\n" sorted))

let solve ~env ~np ~profile_name ~repos_with_shas ?ocaml_version
    ~cache_dir
    ~(opam_commit : Current_git.Commit.t Current.t)
    (tracked : Track.t list Current.t) =
  let open Current.Syntax in
  Current.component "[%s] day11-solve" profile_name
  |>
  let> tracked and> commit = opam_commit in
  let commit_hash = Current_git.Commit.id commit
    |> Current_git.Commit_id.hash in
  let targets = List.map Track.pkg tracked in
  let ocaml_version_str = match ocaml_version with
    | Some pkg -> OpamPackage.to_string pkg
    | None -> "" in
  Solver_cache.get
    { repos_with_shas; env; np; profile_name; ocaml_version; cache_dir }
    SolveOp.Key.{
      targets;
      commit = commit_hash;
      repos_digest = repos_digest repos_with_shas;
      ocaml_version = ocaml_version_str;
    }
  |> Current.Primitive.map_result (Result.map (fun v ->
    List.filter_map (fun (pkg_str, json_str) ->
      try
        let pkg = OpamPackage.of_string pkg_str in
        let json = Yojson.Safe.from_string json_str in
        match Day11_solution.Solve_result.of_json json with
        | Ok result -> Some { target = pkg; solve_result = result }
        | Error _ -> None
      with _ -> None
    ) v.SolveOp.Value.results))
