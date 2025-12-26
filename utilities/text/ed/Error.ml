(*s: Error.ml *)

(*s: exception [[Error.Error]] *)
exception Error of string
(*e: exception [[Error.Error]] *)

(*s: function [[Error.e]] *)
(* the raise will effectively jump on the exn handler in CLI.main()
 * (emulating the longjmp done in C).
 *)
let e s =
  raise (Error s)
(*e: function [[Error.e]] *)
(*e: Error.ml *)
