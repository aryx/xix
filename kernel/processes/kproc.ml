open Common
open Types
open Proc_

(* todo: can call files/chan.ml from here? *)
module Chan = struct
let share chan =
  Ref.inc chan.Chan_.refcnt |> ignore; 
  chan
end

(* less: 
 * let kernel_shared_namespace = ...
 *)

let kproc name f =
  let up = Globals.up () in

  (* I prefer to inline Proc.alloc () here *)
  let pid = Counter.gen Process.pidcounter in
  let p = {
    pid = pid;
    state = Scheding;

    kproc = Some f;

    slash = up.slash;
    dot = Chan.share up.dot;

    (* no need segment, f() uses kernel space memory *)
    seg = Hashtbl.create 1;
    seglock = Qlock.alloc ();

    name = name;
    user = !Globals.eve;

    in_syscall = false;

    parent = None;
    nchild = 0;
    waitq = [];
    childlock = Spinlock.alloc ();
    
    priority = Scheduler_.prioKproc;
    base_priority = Scheduler_.prioKproc;

    thread = Thread.create (fun () -> failwith "TODO:kproc.thread?") ();

    rdz = None; rdzlock = Spinlock.alloc ();
    alarm = None;
    (* todo: misc fields *)
  }
  in
  (* as in Proc.alloc() *)
  Process.hash p;

  (* todo: arch_kprocchild *)
  !Hooks.Scheduler.ready pid
