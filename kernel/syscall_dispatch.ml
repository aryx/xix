open Types

open Syscall

let dispatch syscall =
  match syscall with
  | Nop -> Sysmisc.syscall_nop ()
  | _ -> raise Todo
