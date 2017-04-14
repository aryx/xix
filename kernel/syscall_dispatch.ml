open Common

open Syscall

let dispatch syscall =
  match syscall with
  | Nop -> Sysmisc.syscall_nop ()

  | Brk x -> Sysbrk.syscall_brk x

  | Errstr -> Sysmisc.syscall_errstr ()

  | _ -> raise Todo
