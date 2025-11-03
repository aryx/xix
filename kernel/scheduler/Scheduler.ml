open Common
open Types
open Scheduler_

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)


let thread_wakeup _ = 
  let _ = failwith "TODO" in
  ()

let thread_sleep _ = 
  let _ = failwith "TODO" in
  ()

(*****************************************************************************)
(* Types and globals *)
(*****************************************************************************)

type runq = {
  (* use pid? *)
  queues: (Process_.t Queue.t) array; (* length = Scheduler_.nb_priorities *)
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

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let any_ready () =
  runq.runvec <> 0

(* the priority of a process must be regularly adjusted (to avoid starving) *)
let add (p : Process_.t) prio = 
  Spinlock.lock runq.l;

  p.priority <- prio;
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
  let p : Process_.t = Queue.take runq.queues.(iprio) in
  if p.state <> Process_.Ready
  then failwith (spf "process %s %d in runq was not in Ready state" 
                   p.name p.pid);
  
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
      let _ = failwith "todo: idlehands" in
      ()
    done;
    raise (Impossible "while infinite loop can exit only through raise")
  with Found prio ->
    (* less: splhi? *)
    let p : Process_.t = dequeue prio in
    (* less: loop and dance if runq has changed in between *)
    p.state <- Process_.Scheding;
    (* less: lastcpu *)
    (* less: proctrace strace *)
    Spinlock.unlock runq.l;
    p

(* How to write the scheduler written in OCaml? 
 * You can have one thread for the scheduler (schedinit())
 * and then only one active thread at a time. Then, the active
 * thread can simply call wakeup on the scheduler and sleep which
 * should automatically switch to the now only ready thread: scheduler.
 * 
 * alt: have a special kernel_thread in scheduler.c?
 * the first one created? and delegate to him the scheduling policy?
 *)


(* the big one! *)
let sched () =
  (* todo: check ilockdepth  *)
  (* todo: splhi *)
  (* less: cpu->cs stats *)

  (* todo: delaysched and nlocks (and adjust unlocks to call sched sometimes *)

  (* less: arch_procsave hooks *)
  (* XXX Thread.critical_section := true; *)
  Logs.err (fun m -> m "TODO: Thread.critical_section");
  let cpu = Globals.cpu () in
  thread_wakeup cpu.Cpu.thread;
  thread_sleep (); (* reset Thread.critical_section *)
  (* less: arch_procrestore *)
  (* todo: spllo *)
  ()

(*****************************************************************************)
(* API *)
(*****************************************************************************)

(* from Running to Ready in right priority queue *)
let ready (p : Process_.t) : unit =
  (* less: splhi/splx? why? *)
  (* less: cpu->readied *)
  p.state <- Process_.Ready;
  let prio = p.priority in
  (* less: adjust priority *)
  add p prio

(* The function finally executed by the main kernel thread (in cpu.thread) *)
let _scheduler () =
 let cpu : Cpu.t = Globals.cpu () in
 assert (Thread.id (Thread.self ()) = Thread.id (cpu.thread));
 while true do 
  (*Thread.critical_section := true;*)
  Logs.err (fun m -> m "TODO: Thread.critical_section");
  (* less: assert splhi? *)
  (* less: check ilockdepth  *)
  let up : Process_.t = Globals.up () in
  (match up.state with
  | Process_.Running -> ready up
  | Process_.Moribund -> raise Todo
  | _ -> raise (Impossible "can hve either Running or Moribund in scheduler()")
  );
  cpu.proc <- None;

  (* from now on, up is nil *)
  (* less: call sched()?? better put the logic in scheduler too no? *)

  let p = find_proc () in
  (* less: update priority *)
  (* less: adjust unless readied process in which case use quantum of
   * process that readied it *)
  cpu.sched_ticks <- cpu.ticks + (Arch.hz / 10);
  
  cpu.proc <- Some p;
  (* new up! *)
  let up : Process_.t = Globals.up () in
  up.state <- Process_.Running;
  (* less: up.Proc_.cpu <- cpu.id *)

  (* todo: mmuswitch *)
  thread_wakeup up.thread;
  thread_sleep () (* reset Thread.critical_section *)
 done

(* not super useful *)
let yield () : unit =
  if any_ready () 
  (* less: adjust lastupdate *)
  then sched ()

(* todo: adjust Hooks.Scheduler *)
