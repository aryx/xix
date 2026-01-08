(*s: Error.ml *)

(*s: exception [[Error.Error]] *)
exception Error of string
(*e: exception [[Error.Error]] *)

let e_warn s =
  Logs.warn (fun m -> m "%s" s);
  (* ed: to remain backward compatible *)
  raise (Error "")

let e_err s =
  Logs.err (fun m -> m "%s" s);
  raise (Error "")

(*s: function [[Error.e]] *)
(* the raise will effectively jump on the exn handler in CLI.main()
 * (emulating the longjmp done in C).
 *)
let e_legacy s =
  raise (Error s)
(*e: function [[Error.e]] *)

(*e: Error.ml *)
