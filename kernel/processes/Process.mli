
type t = Process_.t

val pidcounter: Counter.t

val proc_of_pid : Types.pid -> t

val hash: t -> unit
val unhash: t -> unit

type allocator = {
  (* used even more now that I use pid in Qlock.q instead of direct reference*)
  hpids: (Types.pid, Process_.t) Hashtbl.t;

  l: Spinlock_.t;
}

val allocator : allocator
