
type t = {
  (* sorted by fasttk *)
  mutable elts: Timer.t list;

  (* !lock ordering! lock(Timer.t); lock(Timers.t) *)
  l: Ilock.t;
}

val cpus_timers : t array

val add : Timer.t -> unit
