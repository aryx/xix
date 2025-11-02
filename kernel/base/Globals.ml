(* todo: delete once added the { Xxx. } feature in my ocaml light *)
open Cpu 
open Conf
open Process_
open Chan_
open Spinlock_
open Ref_
open Qlock_

(*****************************************************************************)
(* Fakes *)
(*****************************************************************************)

(* less: could move the globals (and fakexxx) in their respective files *)

let fakelock = { Spinlock_.
  hold = ref false;
  p = 0; (* same than fakeproc.pid *)
}
let fakeref = { Ref_.
  cnt = 0;
  Ref_.l = fakelock;
}
let fakeqlock = { Qlock_.
  locked = false;
  q = Queue.create ();
  Qlock_.l = fakelock;
}
let fakeqid = { Chan_.
  qpath = 0;
  qver = 0;
  qtype = QFile;
}
let fakechan = { Chan_.
  chantype = 0;
  qid = fakeqid;
  path = [];
  offset = 0;
  mode = { Chan_.read = false; write = false};
  ismtpt = false;
  refcnt = fakeref;
}

let fakeproc = { Process_.
  pid = 0;
  state = Dead;
  slash = fakechan;
  dot = fakechan;
  seg = Hashtbl.create 11;
  seglock = fakeqlock;
  name = "";
  in_syscall = false;
  parent = None;
  nchild = 0;
  waitq = [];
  childlock = fakelock;
  user = "";
  kproc = None;
  priority = Scheduler_.Prio 0;
  base_priority = Scheduler_.Prio 0;
  thread = Thread.self ();
  rdz = None; rdzlock = fakelock;
  alarm = None;
}

let fakeconf = { Conf.
  ncpu = 0;
  nproc = 0;
  mem = [];

  user_pages = 0;
  kernel_pages = 0;
  npages = 0;
}
 
(*****************************************************************************)
(* !!! The globals !!! *)
(*****************************************************************************)

let cpu0 = { Cpu.
  id = 0;
  proc = None;
  ticks = 0;
  sched_ticks = 0;
  Cpu.thread = Thread.self();
}

let cpus = 
  Array.init Arch.max_cpus (fun i -> if i = 0 then Some cpu0 else None)
(* less: active *)

let devtab = ref ([| |]: Device_.t array)

let conf = ref fakeconf
(* less: let config = Hashtbl.create 101 *)

(* less: changed by writing in /dev/owner? *)
let eve = ref ""

(* A few important globals spread in other files:
 *  - Scheduler.runq priority queues
 *  - Timers.timers array 
 *  - Alarms.alarms list
 *)

(*****************************************************************************)
(* cpu() and up() *)
(*****************************************************************************)

(* less: opti: a special register (faster and local to a processor) *)
let cpu () = 
  (* todo: handle multiple processors *)
  match cpus.(0) with
  | None -> Error.panic "cpu: no processor??"
  | Some x -> x


(* UP for user process?
 * less: opti: a special register (faster and local to a processor) 
 *)
let up () = 
  match (cpu()).proc with
  (* sentinel proc; convenient because need less if (up == nil) code *)
  | None -> fakeproc (* todo? or failwith? *)
  | Some x -> x
