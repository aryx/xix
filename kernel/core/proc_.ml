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

type t = {
  pid: pid;
  mutable state: state;

  mutable user: string;
  (* executable name. Can be also *init* for kernel process *)
  mutable name: string; 

  (* 'mutable' because can not be set in alloc() but fork(). 
   * 'option' because if NoWait flag given, no parent.
   *)
  mutable parent: pid option; 
  mutable nchild: int;

  (* less: should use Segment_.t array; more efficient, but more tedious *)
  mutable seg: (section, Segment_.t) Hashtbl.t;
  (* less: should use monitor instead of separate data and its lock? 
   * or have a mutable seg: Segment_t.array Qlock_.locked; ?
   *)
  seglock: Qlock_.t;

  mutable slash: Chan_.t;
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
