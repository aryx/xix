open Common
open Types
open Scheduler_

type runq = {
  (* use pid? *)
  queues: (Proc_.t Queue.t) array; (* length = Scheduler_.nb_priorities *)
  l: Spinlock_.t;
  (* bitset *)
  mutable runvec: int;
  mutable nb_ready: int;
}

let runq = {
  queues = Array.init nb_priorities (fun _i -> Queue.create () );
  l = Spinlock.alloc();
  runvec = 0;
  nb_ready = 0;
}

let any_ready () =
  runq.runvec <> 0

let add p = 
  Spinlock.lock runq.l;

  (* less: priority can be an argument instead of constant *)
  let (Prio iprio) = p.Proc_.priority in
  
  Queue.add p runq.queues.(iprio);
  runq.nb_ready <- runq.nb_ready + 1;
  runq.runvec <- runq.runvec lor (1 lsl iprio);

  Spinlock.unlock runq.l

let dequeue p =
  raise Todo

exception Found of Proc_.t

let find_proc () =
  (* todo: if cpu->readied *)
  (* less: use Proc.lastcpu and affinity *)
  (* todo: spllo()? *)

  (* less: do not lock runq? do it in 2 steps in a loop? *)
  Spinlock.lock runq.l;

  try 
    while true do 
      for i = Scheduler_.nb_priorities -1 to 0 do
        if Queue.length runq.queues.(i) > 0
        then raise (Found (Queue.take runq.queues.(i)))
      done;
      (* nothing found?? *)
      Spinlock.unlock runq.l;
      failwith "todo: idlehands"
    done;
    raise (Impossible "while infinite loop can exit only through raise")
  with Found p ->
    (* todo: splhi? *)
    p.Proc_.state <- Proc_.Scheding;
    (* less: lastcpu *)
    p

(* the big one! *)
let sched () =
  raise Todo

let schedinit () =
  raise Todo


(* from Running to Ready in right priority queue *)
let ready p =
  (* less: splhi/splx? why? *)
  p.Proc_.state <- Proc_.Ready;
  (* less: adjust priority *)
  add p
