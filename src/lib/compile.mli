(** Compilation step

    The documentation compilation is done as an ocluster. It takes for input one
    prep/ folder and its compiled dependencies. It uses `voodoo-do` to perform
    the compilation, link and html generation steps, outputting the results in
    the compile/ and html/ folders. *)

type hashes = { compile_hash : string; linked_hash : string; html_hash : string }

type t
(** A compiled package *)

val hashes : t -> hashes
(** Hash of the compiled artifacts *)

val blessing : t -> Package.Blessing.t
(** A blessed package is compiled in the compile/packages/... hierarchy, whereas
    a non-blessed package is compiled in the compile/universes/... hierarchy *)

val package : t -> Package.t
(** The compiled package *)

val extract_hashes :
  (Storage.id_hash option * Storage.id_hash option) * string list ->
  string ->
  (Storage.id_hash option * Storage.id_hash option) * string list
(** Function used to parse log lines *)

val v :
  generation:Epoch.t Current.t ->
  config:Config.t ->
  name:string ->
  voodoo:Voodoo.Do.t Current.t ->
  blessing:Package.Blessing.t Current.t ->
  deps:t list Current.t ->
  Prep.t Current.t ->
  t Current.t
(** [v ~voodoo ~cache ~blessed ~deps prep] is the ocurrent component in charge
    of building [prep], using the previously-compiled [deps]. [blessed] contains
    the information to figure out if [prep] is a blessed package or not. [cache]
    contains the artifacts cache metadata to track eventual changes. [voodoo] is
    the voodoo-do tool tracker.

    Notably, if compilation artifacts already exists, then the job is a no-op. *)
