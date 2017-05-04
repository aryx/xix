open Common
open Types

(* less: possible memory leak if a process sets an alarm far away
 * and gets killed in the mean time; its Proc_.t will not be gc'ed.
 * todo: store pid instead in list? and take care to check for Not_found
 *  when use Proc.proc_of_pid.
 *)
type t = {
  (* sorted list by Proc_.alarm time *)
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

let del_proc p =
  assert(not (Qlock.canlock alarms.ql));
  (* todo: remove sorted list *)
  raise Todo

let add_proc p when_ =
  assert(not (Qlock.canlock alarms.ql));
  (* todo: insert sorted list *)
  raise Todo
