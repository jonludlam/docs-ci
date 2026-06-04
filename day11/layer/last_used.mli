(** Per-layer "last used" timestamp for LRU cache eviction.

    Each layer has a [last_used] sentinel file in its directory whose
    mtime records the most recent access. The file's content is
    irrelevant — only its mtime matters.

    This is deliberately split off from {!Meta} so that marking
    a layer as used is cheap — just [utimensat] on a small sentinel
    file, no JSON read/write. *)

val touch : Eio_unix.Stdenv.base -> Fpath.t -> unit
(** [touch env layer_dir] records that the layer has just been accessed.
    Creates [layer_dir/last_used] if it doesn't exist, or updates its
    mtime if it does. Errors are silently ignored — touch must never
    fail a build. *)

val get : Eio_unix.Stdenv.base -> Fpath.t -> float option
(** [get env layer_dir] returns the unix timestamp (seconds since epoch)
    of the last touch, or [None] if the sentinel file doesn't exist
    or can't be stat'd. *)
