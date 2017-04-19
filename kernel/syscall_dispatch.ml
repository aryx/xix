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

  | Exec (cmd, args) -> Sysexec.syscall_exec cmd args

  | Await ->
    let _opt_waitmsg =  Sysawait.syscall_await () in
    (* let (_pid, _time, _msg) = waitmsg    in *)
    raise Todo

  | Exits str ->
    Sysexits.syscall_exits str
    (* no return from here ... *)

  | Errstr -> Syserrstr.syscall_errstr ()

  | _ -> raise Todo
