(*s: Process.mli *)
(*s: type [[Process.waitfor_result]] *)
type waitfor_result =
  | WaitforInterrupted
  | WaitforFound
  | WaitforNotfound
(*e: type [[Process.waitfor_result]] *)

(*s: signature [[Process.return]] *)
val return : < Cap.exit ; .. > -> unit -> unit
(*e: signature [[Process.return]] *)
(*s: signature [[Process.exit]] *)
val exit : < Cap.exit ; .. > -> string -> unit
(*e: signature [[Process.exit]] *)
(*s: signature [[Process.waitfor]] *)
val waitfor : < Cap.wait; .. > -> int (* pid *) -> waitfor_result
(*e: signature [[Process.waitfor]] *)
(*s: signature [[Process.s_of_unix_error]] *)
val s_of_unix_error : Unix.error -> string -> string -> string
(*e: signature [[Process.s_of_unix_error]] *)
(*e: Process.mli *)
