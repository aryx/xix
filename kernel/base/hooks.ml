open Types

module Scheduler = struct
  (* less: opti: direct call, but mutual dependency problem *)
  let (sched: (unit -> unit) ref) = ref (fun () ->
    failwith "sched() not defined"
  )
  
  (* less: opti: use direct Proc_.t ref instead of pid *)
  let (ready: (pid -> unit) ref) = ref (fun _p ->
    failwith "ready() not defined"
  )
  
  (* todo: take rendez vous *)
  let (sleep: ((unit -> bool) -> unit) ref) = ref (fun _f ->
    failwith "sleep() not defined"
  )

  (* todo: take rendez vous *)
  let (wakeup: (unit -> unit) ref) = ref (fun _rendezvous ->
    failwith "wakeup() not defined"

  )
end

module Chan = struct

  (* called 'namec' in 9 *)
  let (chan_of_filename: (filename -> Chan_.t) ref) = ref (fun _file ->
    failwith "chan_of_filename() not defined"
  )

  let (close: (Chan_.t -> unit) ref) = ref (fun _chan ->
    failwith "chan_of_filename() not defined"
  )

end
