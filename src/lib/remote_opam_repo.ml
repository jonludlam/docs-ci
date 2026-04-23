(** Maintain a local clone of a remote opam-repository at a
    user-specified path.

    ocaml-docs-ci reads a list of [{url, path}] pairs from
    [remotes.json] and installs one of these jobs per pair. The job
    performs a [git clone] the first time, and on every subsequent
    schedule tick [git fetch + reset --hard origin/HEAD] — so [path]
    is always a fresh mirror of the remote. Day11 profiles then
    reference [path] as a regular local-path entry; a
    [Current_git.Local] watcher elsewhere in the pipeline picks up
    the updated HEAD via inotify and re-triggers downstream work.

    Keeping this separate from [Day11_batch.Profile] means the profile
    schema stays local-paths-only, and [day11 batch] works off the
    same profile files without ever handling URLs. *)

module Op = struct
  type t = unit

  module Key = struct
    type t = { url : string; path : Fpath.t }
    let digest t = t.url ^ "|" ^ Fpath.to_string t.path
  end

  module Value = struct
    (* Resolved commit SHA after the pull. *)
    type t = string
    let marshal t = t
    let unmarshal t = t
  end

  let id = "docs-ci-remote-opam-repo"
  let auto_cancel = false
  let pp f (key : Key.t) =
    Fmt.pf f "pull %s → %a" key.url Fpath.pp key.path

  let sh_log job fmt =
    Fmt.kstr (fun s ->
      Current.Job.log job "$ %s" s;
      let cmd = ("", [| "/bin/sh"; "-c"; s |]) in
      let open Lwt.Syntax in
      let* output = Lwt_process.pread_lines cmd
        |> Lwt_stream.to_list in
      List.iter (Current.Job.log job "%s") output;
      Lwt.return (Ok (String.concat "\n" output))
    ) fmt

  let run_sh job fmt =
    Fmt.kstr (fun s ->
      Current.Job.log job "$ %s" s;
      let cmd = ("", [| "/bin/sh"; "-c"; s |]) in
      let open Lwt.Syntax in
      let* status = Lwt_process.exec cmd in
      match status with
      | Unix.WEXITED 0 -> Lwt.return (Ok ())
      | Unix.WEXITED n ->
        Lwt.return (Error (`Msg (Printf.sprintf
          "command exited %d: %s" n s)))
      | _ ->
        Lwt.return (Error (`Msg ("command signalled: " ^ s)))
    ) fmt

  let build () job (key : Key.t) =
    let open Lwt.Syntax in
    let* () = Current.Job.start job ~level:Current.Level.Harmless in
    let path = Fpath.to_string key.path in
    let ( let** ) = Lwt_result.bind in
    let result =
      let git_dir = Filename.concat path ".git" in
      let** () =
        if Sys.file_exists git_dir then
          (* Non-destructive sync: [git fetch] + [git merge
             --ff-only]. If the working tree has diverged from
             origin (local commits, uncommitted edits) the merge
             fails and we surface a clear error instead of silently
             destroying the user's work. *)
          let** () = run_sh job "git -C %s fetch --prune origin" path in
          run_sh job "git -C %s merge --ff-only FETCH_HEAD" path
        else
          run_sh job "git clone %s %s"
            (Filename.quote key.url) (Filename.quote path)
      in
      sh_log job "git -C %s rev-parse HEAD" path
    in
    Lwt_result.map (fun output ->
      let sha = String.trim output in
      Current.Job.log job "%a @ %s" Fpath.pp key.path sha;
      sha) result
end

module Cache = Current_cache.Make (Op)

(** [maintain ~schedule ~url ~path] installs a scheduled puller that
    keeps [path] up to date with [url]. Returns the latest commit SHA
    as a [Current.t]. The return value is usually ignored — the
    downstream [Current_git.Local] watcher on [path] detects commit
    changes via inotify independently. *)
let maintain ~schedule ~url ~path : string Current.t =
  let open Current.Syntax in
  Current.component "pull %s" url |>
  let> () = Current.return () in
  Cache.get ~schedule () Op.Key.{ url; path }

(** One remote-mirror entry, passed via [--remote URL=PATH]. *)
type spec = { url : string; path : Fpath.t }

(** Parse a [URL=PATH] argument. The [=] separator is the first
    one in the string, so URLs containing [=] survive (paths
    shouldn't — but if they do, quote them per shell rules). *)
let spec_of_arg s =
  match String.index_opt s '=' with
  | None ->
    Error (`Msg (Printf.sprintf
      "--remote %S: expected URL=PATH" s))
  | Some i ->
    let url = String.sub s 0 i in
    let path = String.sub s (i + 1) (String.length s - i - 1) in
    if url = "" || path = "" then
      Error (`Msg (Printf.sprintf
        "--remote %S: URL and PATH must both be non-empty" s))
    else
      Ok { url; path = Fpath.v path }
