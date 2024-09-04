type repository =
  | HtmlRaw of (Epoch.t * Package.Blessing.t)
  | Linked of (Epoch.t * Package.Blessing.t)
  | Compile of Package.Blessing.t
  | Prep
  | Prep0

val folder : repository -> Package.t -> Fpath.t

module Base : sig
  type repository = HtmlRaw of Epoch.t | Linked of Epoch.t | Compile | Prep | Prep0

  val folder : repository -> Fpath.t
  val generation_folder : Epoch.stage -> Epoch.t -> Fpath.t
end

(* [for_all repo packages command] is a command that executes [command] for all [packages] folders in [repo].
   $1 contains the folder. $2 contains the package id. *)
val for_all : (repository * Package.t) list -> string -> string list

type id_hash = { id : string; hash : string } [@@deriving yojson]

(* print sha256 hash of the files $1 or empty if it doesn't exist as following line:
   <prefix>:$HASH:$2*)
val hash_command : prefix:string -> string

module Tar : sig
  (* print sha256 hash of $1/content.tar or empty if it doesn't exist as following line:
     <prefix>:$HASH:$2*)
  val hash_command : ?extra_files:string list -> prefix:string -> unit -> string
end

(* parse a line created by the previous command *)
val parse_hash : prefix:string -> string -> id_hash option
