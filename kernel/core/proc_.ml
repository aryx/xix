open Types

type pid = Types.pid

type state = 
  | Running

  | Dead
  | Broken
  | Moribund
  | Stopped

  | Ready
  | Scheding

  | Queueing of rw option
(*
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

  | SExtra
  (* less: physical segment extension *)

type t = {
  pid: pid;
  mutable state: state;

  user: string;
  (* executable name. Can be also *init* for kernel process *)
  mutable name: string; 

  (* None when NoWait flag in sysrfork (also first proc has no parent) *)
  parent: pid option; (* less: opti: direct link to parent *)
  mutable nchild: int;

  (* less: opti: should use Segment_.t array; but more tedious *)
  mutable seg: (section, Segment_.t) Hashtbl.t;
  (* less: should use monitor instead of separate data and its lock? 
   * or have a mutable seg: Segment_t.array Qlock_.locked; ?
   *)
  seglock: Qlock_.t;

  slash: Chan_.t;
  mutable dot: Chan_.t;


  mutable in_syscall: bool;
  (* todo: kstack!!
  *)

  (* less: debugging fields
   *  lastlock: Spinlock.t ref;
   *  qpc: kern_addr; (* pc calling last qlock *)
   *)
  (* less:
  nspinlocks: Ref.t;
  *)
}
