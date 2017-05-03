open Common
open Types
open Rendez_

let alloc () = {
  p = None;
  l = Spinlock.alloc ();
}

let sleep rdz fcond =
  (* todo: splhi/splx *)
  (* less: sanity check nlocks *)
  let up = Globals.up () in
  Spinlock.lock rdz.l;
  Spinlock.lock up.Proc_.rdzlock;

  if rdz.p <> None
  then failwith (spf "double sleep for %s %d" up.Proc_.name up.Proc_.pid);
  (* stricter: *)
  if up.Proc_.rdz <> None
  then raise (Impossible "cant have rendez vous and Running");

  rdz.p <- Some up.Proc_.pid;

  let cond = fcond () in
  if cond (* less: up->notepending *)
  then begin
    rdz.p <- None;
    Spinlock.unlock up.Proc_.rdzlock;
    Spinlock.unlock rdz.l;
  end else begin
    (* less: hook proctrace strace *)
    up.Proc_.state <- Proc_.Wakeme;
    up.Proc_.rdz <- Some rdz;

    (* similar to Scheduler.sched () but with extra unlocks *)
       (* less: arch_procsave hooks *)
       let cpu = Globals.cpu () in
       Thread.critical_section := true;
       Thread.wakeup cpu.Cpu.thread;
    Spinlock.unlock up.Proc_.rdzlock;
    Spinlock.unlock rdz.l;
       Thread.sleep (); (* reset Thread.critical_section *)
       (* less: arch_procrestore *)
       (* todo: spllo *)
     (* stricter: *)
     (* TODO: assert(f())? the condition should now be true no? *)
  end
  

let wakeup rdz =
  (* todo: splhi/splx *)
  Spinlock.lock rdz.l;
  let optp = rdz.p in
  optp |> Common.if_some (fun pid ->
    let p = Proc.proc_of_pid pid in
    Spinlock.lock p.Proc_.rdzlock;
    (match p.Proc_.state, p.Proc_.rdz with
    | Proc_.Wakeme, Some r when r == rdz -> ()
    | _ -> 
      failwith "wakeup: inconsistency";
    );
    rdz.p <- None;
    p.Proc_.rdz <- None;
    Scheduler.ready p;
    Spinlock.unlock p.Proc_.rdzlock;
  );
  Spinlock.unlock rdz.l;
  optp
  
