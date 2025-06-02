(*s: Error.mli *)
(*s: signature [[Error.error]] *)
(* Will behave like a 'raise'. Will Return until you reach
 * the interactive thread and set the status to the error argument.
 * Mostly used by the builtins.
 *)
val error : < Cap.exit ; .. > -> string -> unit
(*e: signature [[Error.error]] *)
(*e: Error.mli *)
