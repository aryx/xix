open Types

type t = {
  mutable locked: bool;
  (* less: opti: use pids to avoid mutual deps with Proc, but slower *)
  q: pid Queue.t;

  (* less: debugging fields
   *  pc: kern_addr;
   *)

  l: Spinlock_.t;
}
