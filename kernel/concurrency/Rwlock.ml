open Common
open Types

(* less: put in core/ too? or no mutual deps for Rwlock.t with Proc?
 * less: opti: in that case, replace pid by Proc_.t reference below?
*)
type t = {
  (* instead of a 'locked: bool' *)
  mutable readers: int;
  mutable writer: bool;

  (* the waiting processes (readers and writers). 
   * readers wait when a writer hold the lock and a writer wait
   * when readers hold the lock (or another writer hold the lock).
  *)
  q: pid Queue.t;
  (* the writer *)
  mutable wproc: pid option;

  (* less: debugging:
   * wpc: kern_addr
   *)

  l: Spinlock.t;
}

(* similar to Qlock.lock *)
let rlock (q : t) : unit =
  Spinlock.lock q.l;
  (* todo: when can have not q.writer && queue.length q.q > 0 ???? *)
  if not q.writer && Queue.length q.q = 0
  then begin
    (* no writer, go for it; we accept multiple concurrent readers *)
    q.readers <- q.readers + 1;
    Spinlock.unlock q.l;
  end else begin
    (* a writer was there or a list of waiting process *)
    (* todo: when have no writer but waiting processes?? *)
    let up : Process_.t = Globals.up () in
    Queue.add up.pid q.q;
    up.state  <- Process_.Queueing (Some Process_.Read);
    Spinlock.unlock q.l;
    !Hooks.Scheduler.sched ();
    (* will resume here once woke up by another process *)
  end

let runlock (q : t) : unit =
  Spinlock.lock q.l;
  q.readers <- q.readers - 1;
  (* this implies that there is no writer? *)
  if q.readers > 0 || Queue.length q.q = 0
  then Spinlock.unlock q.l
  else begin
    (* Queue.length q.q <> 0 && q.readers = 0 *)
    raise Todo
  end


let wlock (q : t) : unit =
  Spinlock.lock q.l;
  if q.readers = 0 && not q.writer
  then begin
    (* no one is waiting *)
    let up : Process_.t = Globals.up () in
    q.writer <- true;
    q.wproc <- Some up.pid;
    Spinlock.unlock q.l;
  end else begin
    let up : Process_.t = Globals.up () in
    Queue.add up.pid q.q;
    up.state <- Process_.Queueing (Some Process_.Write);
    Spinlock.unlock q.l;
    !Hooks.Scheduler.sched ();
    (* will resume here once woke up by another process *)
  end

let wunlock (q : t) : unit =
  Spinlock.lock q.l;
  raise Todo




let canrlock _q =
  raise Todo
