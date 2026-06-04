(** Epoch management for atomic documentation deployment.

    An epoch is a versioned collection of documentation artifacts.
    The live symlink points to the current epoch; promotion switches
    it atomically. *)

type t = {
  hash : string;
  dir : Fpath.t;
}

val create : base_dir:Fpath.t -> string -> t
(** [create ~base_dir hash] creates an epoch directory
    [base_dir/epoch-{hash}/] and returns its handle. *)

val promote : base_dir:Fpath.t -> t -> unit
(** [promote ~base_dir epoch] atomically switches the [html-live]
    symlink to point to [epoch]'s html directory. *)

val current : base_dir:Fpath.t -> t option
(** [current ~base_dir] reads the [html-live] symlink and returns
    the current epoch, or [None] if no epoch is live. *)

val gc : base_dir:Fpath.t -> keep:int -> int
(** [gc ~base_dir ~keep] removes old epoch directories, keeping
    the [keep] most recent. Returns the number deleted. *)
