
type t = Ilock_.t

val alloc: unit -> t

val lock: t -> unit
val unlock: t -> unit
