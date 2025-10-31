open Types

type t = {
  mode: mode;
  ns: t_ns; (* of mode *)

  f: callback;

  (* set in add *)
  mutable fasttk: t_fastticks; (* ns converted to fastticks *)

  (* less: opti: direct reference to Timers.t head *)
  (* Some when active timer *)
  mutable cpu: cpuid option; (* index in Timers.timer[] *)

  (* !lock ordering! lock(Timer.t); lock(Timers.t) *)
  (* todo: why need a lock on the timer itself? race on it? between who? *)
  l: Ilock_.t;
}
  and mode = 
    | Relative
    | Periodic
  and callback = 
    | HzClock
    | Callback of (unit -> unit)
