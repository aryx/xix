open Common

exception Error of string

(* this will effectively jump on the exn handler in CLI.main() emulating
 * the equivalent longjmp in C.
 *)
let e s =
  raise (Error s)

(* this will be called from CLI.main() in an handler for the Error exn *)
let error_1 (env : Env.t) (s : string) : unit =
  (* TODO: reset globals too? *)
  output_string env.out (spf "?%s\n" s)
