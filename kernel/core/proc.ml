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

type segment =
  | PStack
  | PText
  | PData
  | PBss

  | PExtra

type t = {
  pid: pid;
  mutable state: state;

  mutable slash: Chan.t;
  mutable dot: Chan.t;

  mutable seg: Segment_.t array; (* length = Obj.tag PExtra *)
  seglock: Qlock_.t;

  (* less: debugging fields
   *  lastlock: Spinlock.t ref;
   *  qpc: kern_addr; (* pc calling last qlock *)
   *)
  (* less:
  nspinlocks: Ref.t;
  *)
}
