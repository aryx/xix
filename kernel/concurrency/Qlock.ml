open Common
open Types
open Qlock_

(* todo:
 * - use a monitor approach instead of fine-grained locks?
 * - statistics
 * less: use Mutex.t from thread library?
 *)

type t = Qlock_.t

let alloc () =
  { locked = false;
    q = Queue.create ();
    l = Spinlock.alloc ();
  }
  

let lock q =
  (* todo: sanity check ilockdepth, nlocks *)
  Spinlock.lock q.l;
  if not q.locked
  then begin
    q.locked <- true;
    Spinlock.unlock q.l;
  end else begin
    let up : Process_.t = Globals.up () in
    Queue.add up.pid q.q;
    up.state  <- Process_.Queueing None;
    Spinlock.unlock q.l;
    !Hooks.Scheduler.sched ();
    (* will resume here once woke up by another process *)
  end


let unlock q =
  Spinlock.lock q.l;
  if not q.locked 
  then failwith "Qlock.unlock called with qlock not held";
  try 
    let pid = Queue.take q.q in
    Spinlock.unlock q.l;
    !Hooks.Scheduler.ready pid
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

(* so much better than those waserror/nexterror/poperror in C *)
let with_lock f x =
  lock x;
  Fun.protect f ~finally:(fun () ->
    unlock x
  )
