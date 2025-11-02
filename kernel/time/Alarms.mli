
type t = {
  (* sorted list by Proc_.alarm time *)
  mutable elts: Process.t list;

  ql: Qlock.t;
}

val alarms : t

val add_proc : Process.t -> 'a -> unit
val del_proc : Process.t -> unit
