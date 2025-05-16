
(* 0 means ok, otherwise mean error (e.g., 2 for "fatal error") *)
type code = int

type t =
  (* code 0 in Unix *)
  | OK
  (* code 1 in Unix. This will lead to a call to Logs.err and an exit 1.
   * Note that This is similar to Plan9's exits() *)
  | Err of string
  (* specific exit code (must be > 0 otherwise use OK) *)
  | Code of code

val show: t -> string

(* The code can also be 0 here; it will be converted back to Ok by catch() *)
exception ExitCode of code

val exit: < Cap.exit; ..> -> t -> unit

(* [catch caps f] will run [f()] and return its exit value
 * but also catch the Exitcode exn [f] may throw and
 * convert the exn to the corresponding exit value.
 *)
val catch : (unit -> t) -> t
