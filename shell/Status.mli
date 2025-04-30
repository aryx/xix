(* helpers to manipulate the "status" special variable (see Var.ml) *)

val setstatus : string -> unit
val getstatus : unit -> string
val concstatus : string -> string -> string
val truestatus : unit -> bool
