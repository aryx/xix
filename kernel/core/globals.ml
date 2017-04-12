
(* TODO: delete once feature *)
open Cpu 
open Proc
open Chan
open Conf

(* less: could move the globals in their respective files *)

let fakecpu = { Cpu.
  cpuno = 0;
  proc = ref None;
  ticks = 0;
  cpumhz = 0;
}
let fakeqid = { Chan.
  qpath = 0;
  qver = 0;
  qtype = QFile;
}
let fakechan = { Chan.
  chantype = 0;
  qid = fakeqid;
  path = [];
  offset = 0;
  mode = ORead;
  ismtpt = false;
}
let fakeproc = { Proc.
  pid = 0;
  state = Dead;
  slash = fakechan;
  dot = fakechan;
}
let fakeconf = { Conf.
  ncpu = 0;
  nproc = 0;
  mem = [];
}
 

let cpu = ref fakecpu
(* less: cpus array *)
(* less: active *)

let up = ref fakeproc

let devtab = 
[|
|]

let conf = ref fakeconf
(* less: let config = Hashtbl.create 101 *)
