open Common
open Types
open Scheduler_

type runq = {
  (* use pid? *)
  queues: (Proc_.t Queue.t) array; (* length = Scheduler_.nb_priorities *)
  l: Spinlock_.t;
  (* bitset *)
  mutable runvec: int;
  mutable nready: int;
}

let runq = {
  queues = Array.init nb_priorities (fun _i -> Queue.create () );
  l = Spinlock.alloc();
  runvec = 0;
  nready = 0;
}

let any_ready () =
  runq.runvec <> 0

(* the priority of a process must be regularly adjusted (to avoid starving) *)
let add p prio = 
  Spinlock.lock runq.l;

  p.Proc_.priority <- prio;
  let (Prio iprio) = prio in
  
  Queue.add p runq.queues.(iprio);
  runq.nready <- runq.nready + 1;
  runq.runvec <- runq.runvec lor (1 lsl iprio);

  Spinlock.unlock runq.l

(* less: take process p as a parameter and check if still in q *)
let dequeue prio =
  (* less: do the canlock, return nil, and loop? or assume caller has lock *)
  if Spinlock.canlock runq.l
  then failwith "Caller should hold runq.l";
  (*less: Spinlock.lock runq.l;*)
  let (Prio iprio) = prio in
  let p = Queue.take runq.queues.(iprio) in
  if p.Proc_.state <> Proc_.Ready
  then failwith (spf "process %s %d in runq was not in Ready state" 
                   p.Proc_.name p.Proc_.pid);
  
  if Queue.length runq.queues.(iprio) = 0
  then runq.runvec <- runq.runvec land (lnot (1 lsl iprio));
  runq.nready <- runq.nready - 1;
  (*less: Spinlock.unlock runq.l;*)
  p

    

exception Found of Scheduler_.priority

let find_proc () =
  (* todo: if cpu->readied *)
  (* less: use Proc.lastcpu and affinity *)
  (* less: spllo()/splhi() dance? improve concurrency? *)

  (* less: do not lock runq? do it in 2 steps in a loop? *)
  Spinlock.lock runq.l;

  try 
    while true do 
      for i = Scheduler_.nb_priorities -1 to 0 do
        if Queue.length runq.queues.(i) > 0
        then raise (Found (Scheduler_.Prio i))
      done;
      (* nothing found?? *)
      Spinlock.unlock runq.l;
      failwith "todo: idlehands"
    done;
    raise (Impossible "while infinite loop can exit only through raise")
  with Found prio ->
    (* less: splhi? *)
    let p = dequeue prio in
    (* less: loop and dance if runq has changed in between *)
    p.Proc_.state <- Proc_.Scheding;
    (* less: lastcpu *)
    (* less: proctrace strace *)
    Spinlock.unlock runq.l;
    p

(* the big one! *)
let sched () =
  (* todo: check ilockdepth  *)
  raise Todo

let scheduler () =
  (* less: assert splhi? *)
  (* less: check ilockdepth  *)
  (* todo: handle Moribund *)
  (* todo: handle Ready? *)

  (* from now on, up is nil *)

  let p = find_proc () in
  (* less: update priority *)
  (* less: adjust schedticks *)
  
  Globals.cpu.Cpu.proc <- Some p;
  let up = Globals.up () in
  up.Proc_.state <- Proc_.Running;
  (* less: up.Proc_.cpu <- cpu.id *)

  (* todo: mmuswitch *)
  raise Todo


(* from Running to Ready in right priority queue *)
let ready p =
  (* less: splhi/splx? why? *)
  p.Proc_.state <- Proc_.Ready;
  (* less: adjust priority *)
  add p
