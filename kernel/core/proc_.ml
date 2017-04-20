open Types

type pid = Types.pid

type state = 
  | Running

  | Dead
  | Broken
  | Moribund

  | Ready
  | Scheding

  | Queueing of rw option
(*
  | Stopped
  | Rendezvous
  | Waitrelease
  | Wakeme
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
  pid: pid;
  mutable state: state;

  (* less: 9 is not really a multi-user OS. No uid/gid. *)
  user: string;
  (* executable name (can be also "*init*" for kernel processes) *)
  mutable name: string; 

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

  slash: Chan_.t;
  mutable dot: Chan_.t;
  (* todo: 
   * - fds
   * - namespace
   *)


  mutable in_syscall: bool;
  (* todo: kstack!!
  *)

  (* less: debugging fields
   *  last_lock: Spinlock.t ref;
   *  last_ilock: Ilock.t ref;
   *  qpc: kern_addr; (* pc calling last qlock *)
   *)
  (* less:
  nspinlocks: Ref.t; (* nilocks is in cpu.ml *)
  *)
}
