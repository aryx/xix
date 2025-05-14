
type code = int

type t =
  (* code 0 in Unix *)
  | OK
  (* code 1 or more in Unix. Note that This is similar to Plan's exits() *)
  | Err of string
  | Code of int

exception Error of string
exception ExitCode of int

val exit: < Cap.exit; ..> -> t -> unit
