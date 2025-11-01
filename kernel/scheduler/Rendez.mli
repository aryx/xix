
type t = Rendez_.t

val alloc: unit -> t

val wakeup: t -> int(* pid?*) option
val sleep: t -> (unit -> bool) (* fcond *) -> unit
