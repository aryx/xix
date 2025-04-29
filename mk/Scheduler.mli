
val nrunning: int ref

val run: 
  < Cap.exec; .. > ->
  Job.t -> unit

val waitup: 
  < Cap.exec; .. > ->
  unit -> unit
