
(* 0 means ok, otherwise mean error (e.g., 2 = fatal error) *)
type code = int

type t =
  (* code 0 in Unix *)
  | OK
  (* code 1 or more in Unix. Note that This is similar to Plan9's exits() *)
  | Err of string
  | Code of int

exception Error of string
exception ExitCode of int

val exit: < Cap.exit; ..> -> t -> unit
