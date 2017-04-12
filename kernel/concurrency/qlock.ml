open Common
open Types

(* todo:
 * - use monitor approach instead of fine-grained locks?
 *)

type t = {
  mutable locked: bool;
  q: Proc.t Queue.t;

  (* less: debugging fields
   *  pc: kern_addr;
   *)

  l: Spinlock.t;
}

let lock q =
  Spinlock.lock q.l;
  if not q.locked
  then begin
    q.locked <- true;
    Spinlock.unlock q.l;
  end else begin
    Queue.add !Globals.up q.q;
    (!Globals.up).Proc.state  <- Proc.Queueing None;
    Spinlock.unlock q.l;
    !Sched.sched ();
    (* will resume here once woke up by another process *)
  end


let unlock q =
  Spinlock.lock q.l;
  if not q.locked 
  then failwith "Qlock.unlock called with qlock not held";
  try 
    let p = Queue.take q.q in
    Spinlock.unlock q.l;
    !Sched.ready p
  with Queue.Empty ->
    q.locked <- false;
    Spinlock.unlock q.l
