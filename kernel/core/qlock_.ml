open Types

(* we can not put this type in concurrency/ because of mutual deps
 * between locks and a proc
 *)
type t = {
  mutable locked: bool;
  (* less: opti: I use pids to avoid mutual deps with Proc, but slower *)
  q: pid Queue.t;

  (* less: debugging fields
   *  pc: kern_addr;
   *)

  l: Spinlock_.t;
}
