(** Lock file tracking and management.

    Each in-progress build acquires a file lock under
    [<cache_dir>/locks/]. This module queries those lock files to report
    what is currently building, and can clean up stale locks left by
    crashed processes. Used by the web dashboard and CLI status commands. *)

(** The build stage a lock belongs to. *)
type stage = Build | Doc | Tool

(** Metadata parsed from a held lock file. *)
type lock_info = {
  stage : stage;
  package : string;
  version : string;
  universe : string option;
  pid : int;
  start_time : float;
  layer_name : string option;
  temp_log_path : string option;
}

(** Return metadata for every currently-held lock in [cache_dir]. *)
val list_active : cache_dir:string -> lock_info list

(** Remove lock files whose owning process is no longer running. *)
val cleanup_stale : cache_dir:string -> unit

(** Test whether the lock at the given path is currently held by a process. *)
val is_lock_held : string -> bool
