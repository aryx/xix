open Common
open Types
open Rendez_

type t = Rendez_.t

let thread_wakeup _ = 
  let _ = failwith "TODO" in
  ()

let thread_sleep _ = 
  let _ = failwith "TODO" in
  ()

let alloc () : t = {
  p = None;
  l = Spinlock.alloc ();
}

let sleep (rdz : Rendez_.t) (fcond : unit -> bool) : unit =
  (* todo: splhi/splx *)
  (* less: sanity check nlocks *)
  let up : Process_.t = Globals.up () in
  Spinlock.lock rdz.l;
  Spinlock.lock up.rdzlock;

  if rdz.p <> None
  then failwith (spf "double sleep for %s %d" up.name up.pid);
  (* stricter: *)
  if up.rdz <> None
  then raise (Impossible "cant have rendez vous and Running");

  rdz.p <- Some up.pid;

  let cond = fcond () in
  if cond (* less: up->notepending *)
  then begin
    rdz.p <- None;
    Spinlock.unlock up.rdzlock;
    Spinlock.unlock rdz.l;
  end else begin
    (* less: hook proctrace strace *)
    up.state <- Process_.Wakeme;
    up.rdz <- Some rdz;

    (* similar to Scheduler.sched () but with extra unlocks *)
       (* less: arch_procsave hooks *)
       let cpu = Globals.cpu () in
       (* XXX: Thread.critical_section := true; *)
       Logs.err (fun m -> m "TODO: critical_section");
       thread_wakeup cpu.Cpu.thread;
    Spinlock.unlock up.rdzlock;
    Spinlock.unlock rdz.l;
       thread_sleep (); (* reset Thread.critical_section *)
       (* less: arch_procrestore *)
       (* todo: spllo *)
     (* stricter: *)
     (* TODO: assert(f())? the condition should now be true no? *)
  end
  

let wakeup (rdz : t) : pid option =
  (* todo: splhi/splx *)
  Spinlock.lock rdz.l;
  let optp = rdz.p in
  optp |> Option.iter (fun pid ->
    let p : Process_.t = Process.proc_of_pid pid in
    Spinlock.lock p.rdzlock;
    (match p.state, p.rdz with
    | Process_.Wakeme, Some r when r == rdz -> ()
    | _ -> 
      failwith "wakeup: inconsistency";
    );
    rdz.p <- None;
    p.rdz <- None;
    Scheduler.ready p;
    Spinlock.unlock p.rdzlock;
  );
  Spinlock.unlock rdz.l;
  optp
