type t

val v : Config.t -> Voodoo.t -> t

type stage = [ `Linked | `Html | `Jsoo ]

val digest : stage -> t -> string

val pp : t Fmt.t
