
type prio = Spl_.prio

val high: unit -> prio
val low: unit -> prio

val set: prio -> unit
val is_low: unit -> bool
