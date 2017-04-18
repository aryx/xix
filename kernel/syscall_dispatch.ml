open Common

open Syscall

(* todo: uniform return value?? catch errors?
*)
let dispatch syscall =
  match syscall with
  | Nop -> Sysnop.syscall_nop ()

  | Brk x -> Sysbrk.syscall_brk x

  | Rfork x -> 
    let _pid = Sysrfork.syscall_rfork x in
    raise Todo

  | Await ->
    let (_pid, _time, _msg) = Sysawait.syscall_await () in
    raise Todo

  | Errstr -> Syserrstr.syscall_errstr ()

  | _ -> raise Todo
