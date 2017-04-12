
let (sched: (unit -> unit) ref) = ref (fun () ->
  failwith "sched() not defined"
)

let (ready: (Proc.t -> unit) ref) = ref (fun p ->
  failwith "ready() not defined"
)
