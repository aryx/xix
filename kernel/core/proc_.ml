open Types

type pid = Types.pid

type state = 
  | Running
  | Ready

  | Dead
  | Moribund (* soon to be Dead but not Dead yet *)
  | Broken (* to debug (=~ core dump) *)

  | Scheding (* soon to be Ready but not Ready yet *)

  | Wakeme (* sleeping *)
  | Queueing of rw option
(*
  | Stopped
  | Rendezvous
  | Waitrelease
*)
  and rw = Read | Write

(* a.k.a segment *)
type section =
  | SText
  | SData
  | SBss
  | SStack

  | SExtra (* temporary SStack used in sysexec *)
  (* less: physical segment extension *)

(* message sent from child to parent through exits() and 
 * received by parent through await().
 *)
type wait_msg = {
  child_pid: pid;
  (* less: child_execname: string so no need put it in string/ *)
  child_exits: string; (* <execname> pid: <exitstr> *)
  (* less: time fields of child *)
}



type t = {
  (* ---------------------------------------------------------------- *)
  (* State *)
  (* ---------------------------------------------------------------- *)
  pid: pid;
  mutable state: state;

  (* less: 9 is not really a multi-user OS. No uid/gid. *)
  user: string;
  (* executable name (can be also "*init*" for kernel processes) *)
  mutable name: string; 

  (* ---------------------------------------------------------------- *)
  (* Process hierarchy *)
  (* ---------------------------------------------------------------- *)

  (* None when NoWait flag in sysrfork (also first proc has no parent) *)
  (* less: opti: direct link to parent *)
  parent: pid option; 
  mutable nchild: int;
  mutable waitq: wait_msg list;
  (* the child of a process will modify nchild and waitq above, as well
   * as calls to rfork and await in the parent, hence the lock below.
   *)
  childlock: Spinlock_.t;
  (* todo: waitr: Rendezvous_.t; *)

  (* ---------------------------------------------------------------- *)
  (* Memory *)
  (* ---------------------------------------------------------------- *)

  (* less: opti: should use Segment_.t array; but more tedious *)
  mutable seg: (section, Segment_.t) Hashtbl.t;
  (* seglock is useful only when you have a pager in a concurrent kernel
   * process that wants to access your process segments.
   * less: why pager wants your segments? 
   *)
  (* less: should use monitor instead of separate data and its lock? 
   *  or have a mutable seg: Segment_t.array Qlock_.locked; ?
   *)
  seglock: Qlock_.t;

  (* ---------------------------------------------------------------- *)
  (* Scheduling *)
  (* ---------------------------------------------------------------- *)

  mutable priority: Scheduler_.priority;
  base_priority: Scheduler_.priority;
  (* less: fixedpri *)

  thread: Thread.t;

  (* ---------------------------------------------------------------- *)
  (* Files *)
  (* ---------------------------------------------------------------- *)

  slash: Chan_.t;
  mutable dot: Chan_.t;
  (* todo: 
   * - fds
   * - namespace
   *)

  (* ---------------------------------------------------------------- *)
  (* Synchronization *)
  (* ---------------------------------------------------------------- *)
  mutable rdz: Rendez_.t option;
  rdzlock: Spinlock_.t;

  (* ---------------------------------------------------------------- *)
  (* Misc *)
  (* ---------------------------------------------------------------- *)

  mutable in_syscall: bool;

  (* todo: kstack!! via Thread.t? 
  *)

  (* kernel process *)
  kproc: (unit -> unit) option;

  (* less: debugging fields
   *  last_lock: Spinlock.t ref;
   *  last_ilock: Ilock.t ref;
   *  qpc: kern_addr; (* pc calling last qlock *)
   *)
  (* less:
  nspinlocks: Ref.t; (* nilocks is in cpu.ml *)
  *)
}
