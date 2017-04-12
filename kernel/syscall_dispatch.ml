open Common

open Syscall

let dispatch syscall =
  match syscall with
  | Nop -> Sysmisc.syscall_nop ()

  | Errstr -> Sysmisc.syscall_errstr ()

  | _ -> raise Todo
