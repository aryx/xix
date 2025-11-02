open Common
open Types

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Types and globals *)
(*****************************************************************************)

(* less: possible memory leak if a process sets an alarm far away
 * and gets killed in the mean time; its Proc_.t will not be gc'ed.
 * todo: store pid instead in list? and take care to check for Not_found
 *  when use Proc.proc_of_pid.
 *)
type t = {
  (* sorted list by Proc_.alarm time *)
  mutable elts: Process_.t list;

  ql: Qlock.t;
}

let alarms = {
  elts = [];
  ql = Qlock.alloc ();
}

(*****************************************************************************)
(* API *)
(*****************************************************************************)

(* for alarm_kproc and hz_checkalarms to sleep/wakeup *)
let _rendez = 
  Rendez.alloc ()

let del_proc (_p : Process.t) : unit =
  assert(not (Qlock.canlock alarms.ql));
  (* todo: remove sorted list *)
  let _ = raise Todo in
  ()

let add_proc (_p : Process.t) (_when_ : 'a) : unit =
  assert(not (Qlock.canlock alarms.ql));
  (* todo: insert sorted list *)
  let _ = raise Todo in
  ()
