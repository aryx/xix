val nrunning : int ref
val run : < Cap.fork ; Cap.exec ; .. > -> Job.t -> unit
val waitup : < Cap.fork ; Cap.exec ; .. > -> unit -> unit
