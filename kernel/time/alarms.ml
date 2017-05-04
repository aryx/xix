open Common
open Types

type t = {
  mutable elts: Proc_.t list;

  ql: Qlock.t;
}

let alarms = {
  elts = [];
  ql = Qlock.alloc ();
}

(* for alarm_kproc and hz_checkalarms to sleep/wakeup *)
let rendez = 
  Rendez.alloc ()
  
