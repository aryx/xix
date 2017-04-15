open Common
open Types
open Proc_


let syscall_rfork flags =
  match flags with
  | Syscall.Fork (fork_flags, flags) ->
    let up = !(Globals.up) in

    (* I prefer to inline Proc.alloc () here *)
    let pid = Counter.gen Proc.pidcounter in
    let p = {
      pid = pid;
      state = Scheding;

      parent = Some up.pid;
      nchild = 0;

      slash = up.slash;
      dot = begin up.dot (* todo: increment ref! *) end;

      (* memory *)
      seg = raise Todo;
      seglock = Qlock.alloc ();

      (* todo: fds *)
      (* todo: namespace *)
      (* todo: environment *)
      (* todo: rendezvous group *)
      (* todo: note group *)

      (* misc *)
      user = up.user;
      name = up.name;

      in_syscall = false;

      (* todo: more misc *)
    }
    in
    (* as in Proc.alloc() *)
    Proc.hash p

  | Syscall.NoFork (flags) -> 
    let up = !(Globals.up) in
    raise Todo
