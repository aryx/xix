open Common
open Types
open Qlock_

(* todo:
 * - use a monitor approach instead of fine-grained locks?
 *)

type t = Qlock_.t

let alloc () =
  { locked = false;
    q = Queue.create ();
    l = Spinlock.alloc ();
  }
  

let lock q =
  Spinlock.lock q.l;
  if not q.locked
  then begin
    q.locked <- true;
    Spinlock.unlock q.l;
  end else begin
    Queue.add !Globals.up.Proc_.pid q.q;
    (!Globals.up).Proc_.state  <- Proc_.Queueing None;
    Spinlock.unlock q.l;
    !Hooks.sched ();
    (* will resume here once woke up by another process *)
  end


let unlock q =
  Spinlock.lock q.l;
  if not q.locked 
  then failwith "Qlock.unlock called with qlock not held";
  try 
    let pid = Queue.take q.q in
    Spinlock.unlock q.l;
    !Hooks.ready pid
  with Queue.Empty ->
    q.locked <- false;
    Spinlock.unlock q.l

let canlock q =
  if not (Spinlock.canlock q.l)
  then false
  else 
    if q.locked 
    then begin
      Spinlock.unlock q.l;
      false
    end else begin
      q.locked <- true;
      Spinlock.unlock q.l;
      true
    end

let with_lock f x =
  lock x;
  Common.finalize f (fun () ->
    unlock x
  )
