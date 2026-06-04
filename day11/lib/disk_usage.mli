(** Disk usage reporting by category. *)

type report = {
  base : int;
  builds : int;
  docs : int;
  jtw : int;
  solutions : int;
  logs : int;
  packages : int;
  total : int;
}

val scan : os_dir:Fpath.t -> cache_dir:Fpath.t -> report
(** [scan ~os_dir ~cache_dir] computes disk usage in bytes for
    each category. *)

val pp : report Fmt.t
(** Pretty-print a report with human-readable sizes. *)
