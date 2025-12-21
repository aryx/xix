open Common

exception Error of string

(* the raise will effectively jump on the exn handler in CLI.main()
 * (emulating the longjmp done in C).
 *)
let e s =
  raise (Error s)

(* this will be called from CLI.main() in an handler for the Error exn *)
let error_1 (e : Env.t) (s : string) : unit =
  (* TODO: reset globals too? *)
  Out.putchr e '?';
  Out.putst e s;
  ()
