open Common
open Types

type t = {
  mode: timer_mode;
  ns: t_ns; (* of mode *)

  (* set in add *)
  mutable fasttk: t_fastticks; (* ns converted to fastticks *)

  (* less: opti: direct reference to Timers.t head *)
  (* todo: cpu: cpuid? so can find back it in Timers.timers array *)

  l: Ilock.t;
}
  and timer_mode = 
    | Relative
    | Periodic

let alloc mode ns =
  { mode = mode;
    ns = ns;
    fasttk = 0;
    l = Ilock.alloc ();
  }


let add timer xs =
  (* less: assert timer is locked? *)

  (match timer.mode with
  | Relative -> 
    (* stricter: *)
    if timer.ns <= 0
    then failwith "timer going in the past";
    timer.fasttk <- Time.fastticks () + Time.ns_to_fastticks timer.ns;
  | Periodic ->
    if timer.ns < 100000
    then failwith "Periodic timer must be at least 100 micro seconds";
    (* less: combine with identical timer *)
    timer.fasttk <- Time.fastticks () + Time.ns_to_fastticks timer.ns;
  );
  (* insert in sorted list *)
  raise Todo

let del timer xs =
  raise Todo
