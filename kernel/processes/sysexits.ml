open Common
open Types
open Proc_

let (hooks: (Proc_.t -> unit) list ref) = ref []

(* in C the str can be null pointer but better use empty string for that *)
let syscall_exits str =

  let up = Globals.up () in
  (* in C the code looks if parent is nil, but I use only parentpid
   * and so parent is nil can also mean RFNOWAIT so I instead
   * look if pid is 1 to check if boot process.
   *)
  if up.pid = 1
  then Error.panic ("boot process died: " ^ str);

  (* exit/await and my parent *)
  up.parent |> Option.iter (fun parent_pid ->
   try 
    let parent = Process.proc_of_pid parent_pid in
    (* note that in between maybe the parent exited and so we may have
     * a reference to a dead parent. Still, the GC should not have
     * collected it yet so we can still use lock on it.
     * todo: the C code rely on parent and parentpid and procarena
     *  and ability to use spinlock on process.exl. My design works too?
     *)
    Spinlock.lock parent.childlock;

    (* less: what about Moribund? *)
    if parent.state <> Proc_.Broken 
    then begin
      parent.nchild <- parent.nchild - 1;
      (* less: update time info in parent *)

      (* to avoid accumulate wait records in badly written daemons *)
      if List.length parent.waitq < 128
      then begin 
        let wmsg = { 
          child_pid = up.pid;
          child_exits = Printf.sprintf "%s %d: %s" up.name up.pid str;
        }
        in
        parent.waitq <- wmsg :: parent.waitq;
        !Hooks.Scheduler.wakeup () (* todo: p.waitr *);
      end
    end;
    Spinlock.unlock parent.childlock;
   with Not_found -> 
     (* parent died *)
     ()
  );

  (* exit/await and my (orphan) children *)
  Spinlock.lock up.childlock;
  (* so my children will not find me anymore *)
  Process.unhash up;
  (* todo: why need that? who waits for me ? for /proc/x/wait/ *)
  (* Hook.Scheduler.wakeup () (* todo: up.waitr *);
  *)
  Spinlock.unlock up.childlock;

  (* free resources *)
  up.seglock |> Qlock.with_lock (fun () ->
    up.seg |> Hashtbl.iter (fun _sec seg ->
      Segment.free seg
    );
    Hashtbl.clear up.seg;
  );
  !hooks |> List.iter (fun f -> f up);

  (* todo: why need that?? coupling with sched *)
  Spinlock.lock Process.allocator.Process.l;
  Spinlock.lock Page.allocator.Page.l;

  up.state <- Proc_.Moribund;
  (* todo: go directly to schedinit instead? *)
  !Hooks.Scheduler.sched ();
  Error.panic "syscall_exits: should never each this point"
