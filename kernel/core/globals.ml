
(* todo: delete once added the { Xxx. } feature in my ocaml light *)
open Cpu 
open Conf
open Proc_
open Chan_
open Spinlock_
open Ref_
open Qlock_

(* less: could move the globals (and fakexxx) in their respective files *)

let fakecpu = { Cpu.
  cpuno = 0;
  proc = ref None;
  ticks = 0;
  cpumhz = 0;
}

let fakelock = { Spinlock_.
  hold = ref false;
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
  mode = ORead;
  ismtpt = false;
  refcnt = fakeref;
}

let fakeproc = { Proc_.
  pid = 0;
  state = Dead;
  slash = fakechan;
  dot = fakechan;
  seg = Hashtbl.create 0;
  seglock = fakeqlock;
  name = "";
  in_syscall = false;
  parent = -1;
}
let fakeconf = { Conf.
  ncpu = 0;
  nproc = 0;
  mem = [];

  upages = 0;
  kpages = 0;
  npages = 0;
}
 

let cpu = ref fakecpu
(* less: cpus array *)
(* less: active *)

let up = ref fakeproc

let devtab = ref ([| |]: Device_.t array)

let conf = ref fakeconf
(* less: let config = Hashtbl.create 101 *)
