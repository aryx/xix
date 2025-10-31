open Common

open Syscall
module R = Syscall.Request
module A = Syscall.Answer

let dispatch syscall =
 try (
  match syscall with
  | R.Nop -> Sysnop.syscall_nop (); A.Void

  | R.Brk x -> Sysbrk.syscall_brk x; A.Void

  | R.Rfork x -> 
    let optpid = Sysrfork.syscall_rfork x in
    A.Rfork optpid

  | R.Exec (cmd, args) -> Sysexec.syscall_exec cmd args; A.Void

  | R.Await ->
    let waitmsg =  Sysawait.syscall_await () in
    A.Await waitmsg

  | R.Exits str ->
    (* no return from here ... *)
    let _ = Sysexits.syscall_exits str in
    (* should never reach here *)
    A.Void

  | R.Sleep ms -> 
       Syssleep.syscall_sleep ms; A.Void
  | R.Alarm ms -> 
    let old_remaining = Sysalarm.syscall_alarm ms in 
    A.Alarm old_remaining

  | _ -> raise Todo
 ) 
 with 
   | Failure s -> 
     A.Error ("Failure: " ^ s)
   (* todo: 
    * Error.Error x -> Error.string_of_error 
    * or can even spread error.ml in different modules
    * and do like in ocaml
    *)
   | exn -> 
     Common2.print ("unexpected exception");
     raise exn
