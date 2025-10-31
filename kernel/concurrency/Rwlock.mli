type t

val rlock: t -> unit
val runlock: t -> unit

val wlock: t -> unit
val wunlock: t -> unit

val canrlock: t -> bool
