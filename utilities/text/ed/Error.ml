(*s: Error.ml *)
(*s: exception [[Error.Error]] *)
exception Error of string
(*e: exception [[Error.Error]] *)

(*s: function [[Error.e_warn]] *)
let e_warn s =
  Logs.warn (fun m -> m "%s" s);
  (* ed: to remain backward compatible *)
  raise (Error "")
(*e: function [[Error.e_warn]] *)
(*s: function [[Error.e_err]] *)
let e_err s =
  Logs.err (fun m -> m "%s" s);
  raise (Error "")
(*e: function [[Error.e_err]] *)

(*s: function [[Error.e_legacy]] *)
(* the raise will effectively jump on the exn handler in CLI.main()
 * ed: we emulate the longjmp done in C.
 *)
let e_legacy s =
  raise (Error s)
(*e: function [[Error.e_legacy]] *)
(*e: Error.ml *)
