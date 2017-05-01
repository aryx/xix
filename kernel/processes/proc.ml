open Common
open Types
open Proc_

type allocator = {
  (* less: an arena allocator? but then Proc.pid can not be a constant field.
   * I think an arena allocator is not necessary here 
   * (the Page arena allocator is necessary on the opposite).
   *)

  (* used even more now that I use pid in Qlock.q instead of direct reference*)
  hpids: (pid, Proc_.t) Hashtbl.t;

  l: Spinlock_.t;
}

(* pids will start at 1, so no conflict with Globals.fake_proc pid 0 *)
let pidcounter = 
  Counter.alloc 0

(* less: let inuse = Ref.alloc ()
 *  and make sure < Globals.conf.Conf.nproc
 *)

let allocator = {
  hpids = Hashtbl.create 101;
  l = Spinlock.alloc ();
}

let hash p =
  Spinlock.lock allocator.l;
  Hashtbl.add allocator.hpids p.Proc_.pid p;
  Spinlock.unlock allocator.l

let unhash p =
  Spinlock.lock allocator.l;
  Hashtbl.remove allocator.hpids p.Proc_.pid;
  Spinlock.unlock allocator.l

(* can raise Not_found *)
let proc_of_pid pid =
  allocator.l |> Spinlock.with_lock (fun () ->
    Hashtbl.find allocator.hpids pid
  )

  
  
(* alloc() is sometimes better inlined in the caller (e.g., in sysrfork)
 * so it's easier to see if you setup everything.
 * less: use { (alloc()) with ... } if you need to modify the non-mutable
 *  fields? but then need a rehash function.
 *)
let alloc () =
  let pid = Counter.gen pidcounter in
  let p = 
  (* less: if can not alloc, do noprocpanic and resrcwait? 
   * (assume use proc arena for that?)
   *)
  { pid = pid;
    state = Scheding;

    slash = Globals.fakechan;
    dot = Globals.fakechan;

    seg = Hashtbl.create 10;
    seglock = Qlock.alloc ();

    name = "";
    user = "*nouser*";
    in_syscall = false;

    parent = None;
    nchild = 0;
    waitq = [];
    childlock = Spinlock.alloc ();

    kproc = None;
    
    priority = Scheduler_.prioNormal;
    base_priority = Scheduler_.prioNormal;
  }
  in
  hash p

(* todo: sysexits just set to Moribund. Who really free?
 * pidunhash and the Gc?
 *)
let free p =
  raise Todo
