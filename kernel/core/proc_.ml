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

type section =
  | SText
  | SData
  | SBss
  | SStack

  | SExtra

type t = {
  pid: pid;
  mutable state: state;
  (* less: user: string; *)

  mutable parent: pid; (* mutable because can not be set in alloc() but fork()*)

  mutable slash: Chan_.t;
  mutable dot: Chan_.t;

  (* less: should use Segment_.t array; more efficient, but more tedious *)
  mutable seg: (section, Segment_.t) Hashtbl.t;
  (* less: should use monitor instead of separate data and its lock? 
   * or have a mutable seg: Segment_t.array Qlock_.locked; ?
   *)
  seglock: Qlock_.t;


  mutable in_syscall: bool;
  mutable name: string; (* can be also *init* for kernel process *)

  (* less: debugging fields
   *  lastlock: Spinlock.t ref;
   *  qpc: kern_addr; (* pc calling last qlock *)
   *)
  (* less:
  nspinlocks: Ref.t;
  kstack??
  *)
}
