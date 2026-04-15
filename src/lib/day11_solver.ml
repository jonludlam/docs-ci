(** Day11-based solver: in-process solving via solver_pool subprocesses.

    Replaces the Cap'n Proto solver service. Uses day11's solver_worker
    binaries for parallel solving, communicating via JSONL files. *)

module SolveOp = struct
  type t = {
    opam_repo : string;
    repos_with_shas : (string * string) list;
  }

  module Key = struct
    type t = {
      targets : OpamPackage.t list;
      commit : string;
    }

    let digest t = t.commit ^ ":" ^
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

  let build (ctx : t) job (key : Key.t) =
    let open Lwt.Syntax in
    let* () = Current.Job.start job ~level:Current.Level.Mostly_harmless in
    Current.Job.log job "Solving %d packages (commit %s)"
      (List.length key.targets)
      (String.sub key.commit 0 (min 12 (String.length key.commit)));
    (* Run in systhread so blocking Unix calls (create_process,
       waitpid) don't freeze the Eio scheduler / Lwt web server.
       Progress callback writes to the job log after each worker. *)
    Lwt_eio.run_eio @@ fun () ->
    let results =
      Eio_unix.run_in_systhread @@ fun () ->
      Day11_solver_pool.Solver_pool.solve_many
        ~on_progress:(fun ~done_count ~total ->
          Current.Job.log job "Solving: %d/%d" done_count total)
        ~np:4 ~repos:ctx.repos_with_shas key.targets
    in
    let result_pairs = List.filter_map (fun (pkg, result) ->
      match result with
      | Ok solve_result ->
        let json = Day11_solution.Solve_result.to_json solve_result in
        Some (OpamPackage.to_string pkg, Yojson.Safe.to_string json)
      | Error _ -> None
    ) results in
    Current.Job.log job "Solved: %d/%d succeeded"
      (List.length result_pairs) (List.length key.targets);
    Ok Value.{ results = result_pairs }
end

module Solver_cache = Current_cache.Make (SolveOp)

type solution = {
  target : OpamPackage.t;
  solve_result : Day11_solution.Solve_result.t;
}

(** Solve all tracked packages using day11's solver. Returns solutions
    keyed by target package. *)
let solve ~opam_repo ~repos_with_shas
    ~(opam_commit : Current_git.Commit.t Current.t)
    (tracked : Track.t list Current.t) =
  let open Current.Syntax in
  Current.component "day11-solve"
  |>
  let> tracked and> commit = opam_commit in
  let commit_hash = Current_git.Commit.id commit
    |> Current_git.Commit_id.hash in
  let targets = List.map Track.pkg tracked in
  Solver_cache.get { opam_repo; repos_with_shas }
    SolveOp.Key.{ targets; commit = commit_hash }
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
