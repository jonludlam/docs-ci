type t [@@deriving yojson]

val digest : t -> string
val pkg : t -> OpamPackage.t

val v :
  repo_label:string ->
  limit:int option ->
  filter:string list ->
  Current_git.Commit.t Current.t ->
  t list Current.t
(** [repo_label] is a stable human-readable identifier for the repo
    — typically its filesystem path. It goes into the OCurrent
    component label so the same (filter, limit) can feed from
    multiple repos across one or more profiles without colliding. *)

val merge : t list Current.t list -> t list Current.t
(** Union per-repo tracking results with "later repos override"
    semantics, keyed by [(package_name, version)]. Mirrors opam's
    overlay resolution — an overlay repo that re-publishes the same
    [name.version] with modified content wins over mainline, while
    packages only in the overlay get added to the tracked universe. *)

module Map : OpamStd.MAP with type key = t
