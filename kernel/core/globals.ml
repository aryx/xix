
(* TODO: delete once feature *)
open Cpu 
open Proc
open Chan

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
  slash = ref fakechan;
  dot = ref fakechan;
}

let cpu = ref fakecpu

(* less: cpus array *)
(* less: active *)

let up = ref fakeproc


let devtab = 
[|
|]
