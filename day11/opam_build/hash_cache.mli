(** Memoized hash computation for layer cache keys.

    Caches opam file hashes and layer hashes to avoid redundant
    computation when building many packages with shared dependencies. *)

type t
(** A hash cache instance. *)

val create :
  find_opam:(OpamPackage.t -> OpamFile.OPAM.t option) ->
  ?patches:Patches.t -> unit -> t
(** [create ~find_opam ?patches ()] creates a new hash cache.
    When [patches] is provided, patch content is incorporated into
    package hashes so patched builds get distinct cache keys. *)

val pkg_opam_hash : t -> OpamPackage.t -> string

val layer_hash : t -> base_hash:string -> OpamPackage.t list -> string
(** [layer_hash t ~base_hash pkgs] returns a hash for a build layer.
    Depends on the base image hash and each package's effective opam
    content. Memoized per package list. *)
