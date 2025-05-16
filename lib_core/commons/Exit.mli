
(* 0 means ok, otherwise mean error (e.g., 2 for "fatal error") *)
type code = int

type t =
  (* code 0 in Unix *)
  | OK
  (* code 1 in Unix. This will lead to a call to Logs.err and an exit 1.
   * Note that This is similar to Plan9's exits() *)
  | Err of string
  (* specific exit code *)
  | Code of int

exception Error of string
exception ExitCode of int

val exit: < Cap.exit; ..> -> t -> unit

(* [catch caps f] will run [f()] and call exit with its exit value
 * but also catch Error and Exitcode exns [f] may throw and
 * exit also with the corresponding exit value.
 *)
val catch : < Cap.exit; ..> -> (unit -> t) -> unit
