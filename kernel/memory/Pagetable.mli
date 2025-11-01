
type t = Pagetable_.t

val alloc: unit -> t
val free: t -> unit
val copy: t -> t
