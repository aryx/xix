
(* Will behave like a 'raise'. Will Return until you reach
 * the interactive thread and set the status to the error argument.
 * Mostly used by the builtins.
 *)
val error: string -> unit
