open Types

let (sched: (unit -> unit) ref) = ref (fun () ->
  failwith "sched() not defined"
)

let (ready: (Proc.t -> unit) ref) = ref (fun p ->
  failwith "ready() not defined"
)

(* because use pid instead of Proc pointer in core/ to avoid
 * big mutual deps
 *)
let (ready_pid: (pid -> unit) ref) = ref (fun pid ->
  failwith "ready_pid() not defined"
)
