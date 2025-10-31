
type t = Spinlock_.t

val alloc: unit -> t

val lock: t -> unit
val unlock: t -> unit
val canlock: t -> bool
val with_lock : (unit -> 'a) -> t -> 'a
