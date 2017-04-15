open Types

(* less: opti: direct call, but mutual dependency problem *)
let (sched: (unit -> unit) ref) = ref (fun () ->
  failwith "sched() not defined"
)

(* less: opti: use direct Proc_.t ref instead of pid *)
let (ready: (pid -> unit) ref) = ref (fun p ->
  failwith "ready() not defined"
)
