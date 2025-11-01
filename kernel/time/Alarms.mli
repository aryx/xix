type t = {
  (* sorted list by Proc_.alarm time *)
  mutable elts: Process_.t list;

  ql: Qlock.t;
}

val alarms : t

val add_proc : Process_.t -> 'a -> unit
val del_proc : Process_.t -> unit
