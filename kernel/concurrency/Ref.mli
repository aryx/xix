type t = Ref_.t

val alloc: int -> t

val inc: t -> int
val dec: t -> int

val dec_and_is_zero : t -> bool

val lock: t -> unit
val unlock: t -> unit
