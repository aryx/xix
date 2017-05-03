open Types

type t = {
  mode: timer_mode;
  ns: t_ns; (* of mode *)

  f: callback;

  (* set in add *)
  mutable fasttk: t_fastticks; (* ns converted to fastticks *)

  (* less: opti: direct reference to Timers.t head *)
  mutable cpu: cpuid option; (* index in Timers.timer[] *)

  (* !lock ordering! lock(Timer.t); lock(Timers.t) *)
  l: Ilock.t;
}
  and timer_mode = 
    | Relative
    | Periodic
  and callback = 
    | HzClock
    | Callback of (unit -> unit)
