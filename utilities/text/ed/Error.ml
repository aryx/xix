
exception Error of string

(* the raise will effectively jump on the exn handler in CLI.main()
 * (emulating the longjmp done in C).
 *)
let e s =
  raise (Error s)
