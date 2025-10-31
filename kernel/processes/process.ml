open Common
open Types
open Process_

type allocator = {
  (* less: opti: an arena allocator? 
   * but then Proc.pid can not be a constant field.
   * I think an arena allocator is not necessary here 
   * (the Page arena allocator is necessary on the opposite).
   *)

  (* used even more now that I use pid in Qlock.q instead of direct reference*)
  hpids: (pid, Process_.t) Hashtbl.t;

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
  Hashtbl.add allocator.hpids p.pid p;
  Spinlock.unlock allocator.l

let unhash p =
  Spinlock.lock allocator.l;
  (* this should garbage collect the Proc, unless it is still referenced
   * in some globals (e.g., timers? alarms?)
   *)
  Hashtbl.remove allocator.hpids p.pid;
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

    seg = Hashtbl.create 10; seglock = Qlock.alloc ();

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

    thread = Thread.create (fun () -> failwith "todo: alloc().thread") ();

    rdz = None; rdzlock = Spinlock.alloc ();
    alarm = None;
  }
  in
  hash p

(* todo: sysexits just set to Moribund. 
 * schedinit really frees!
 * todo: pidunhash and let the Gc do its job?
 *)
let free _p =
  raise Todo
