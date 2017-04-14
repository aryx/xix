open Types

type t = {
  mutable locked: bool;
  (* use pids to avoid mutual deps with Proc *)
  q: pid Queue.t;

  (* less: debugging fields
   *  pc: kern_addr;
   *)

  l: Spinlock_.t;
}
