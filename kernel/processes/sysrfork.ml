open Common
open Types
open Proc_

(* todo: can call files/chan.ml from here? *)
module Chan = struct
let share chan =
  Ref.inc chan.Chan_.refcnt; 
  chan
end


let syscall_rfork flags =
  match flags with
  | Syscall.Fork (fork_flags, flags) ->
    let up = !(Globals.up) in

    (* I prefer to inline Proc.alloc () here (I could use { alloc () with }) *)
    let pid = Counter.gen Proc.pidcounter in

    let seg = Hashtbl.create 10 in
    let p = {
      pid = pid;
      state = Scheding;

      parent = if fork_flags.Syscall.wait_child then Some up.pid else None;
      nchild = 0;
      waitq = [];
      childlock = Spinlock.alloc ();

      slash = up.slash; (* no need to refcount slash *)
      dot = Chan.share up.dot;

      (* memory *)
      seg = begin
        (* todo: seglock |> Qlock.with_lock ?? why need that? because pager? *)
        up.seg |> Hashtbl.iter (fun section s ->
          Hashtbl.add seg section 
            (* we will always share SText and create fresh stack, but
             * we might share SData and SBss
             * todo: remove Segment_.kind and inline some of the code
             * of Segment.dupseg here?
             *)
            (if fork_flags.Syscall.share_mem
             then Segment.share s
             else Segment.copy s
            )
        );
        seg
      end;

      seglock = Qlock.alloc ();

      (* todo: fds *)
      (* todo: namespace *)
      (* todo: environment *)

      (* less: rendezvous group *)
      (* less: note group *)

      (* misc *)
      user = up.user;
      name = up.name;

      in_syscall = false;

      (* todo: more misc *)
    }
    in
    (* as in Proc.alloc() *)
    Proc.hash p;

    if fork_flags.Syscall.wait_child
    then up.childlock |> Spinlock.with_lock (fun () ->
      up.nchild <- up.nchild + 1;
    );

    (* todo: arch_flushmmu, arch_forkchild *)
    !Hooks.Scheduler.ready pid;
    !Hooks.Scheduler.sched ();
    pid

  | Syscall.NoFork (flags) -> 
    let up = !(Globals.up) in
    raise Todo
