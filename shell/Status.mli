(*s: Status.mli *)
(*s: signature [[Status.setstatus]] *)
(* helpers to manipulate the "status" special variable (see Var.ml) *)

val setstatus : string -> unit
(*e: signature [[Status.setstatus]] *)
(*s: signature [[Status.getstatus]] *)
val getstatus : unit -> string
(*e: signature [[Status.getstatus]] *)
(*s: signature [[Status.concstatus]] *)
val concstatus : string -> string -> string
(*e: signature [[Status.concstatus]] *)
(*s: signature [[Status.truestatus]] *)
val truestatus : unit -> bool
(*e: signature [[Status.truestatus]] *)
(*e: Status.mli *)
