open Common
open Types
open Proc_

(* in C the str can be null pointer but better use empty string *)
let syscall_exits str =

  let up = !Globals.up in
  (* in C it looks if parent is nil, but I use only parentpid
   * and so parent is nil can also mean RFNOWAIT so I instead
   * look if pid is 1 to check if boot process.
   *)
  if up.pid = 1
  then Error.panic ("boot process died: " ^ str);

  up.parent |> Common.if_some (fun parent ->
    raise Todo
  );

  (* todo: why need that?? *)
  Spinlock.lock Proc.allocator.Proc.l;
  Spinlock.lock Page.allocator.Page.l;

  up.state <- Proc_.Moribund;
  !Hooks.Scheduler.sched ();
  Error.panic "syscall_exits: should never each this point";
