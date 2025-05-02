val nrunning : int ref

(* need Cap.env for NPROC *)
val run : < Shell.caps ; Cap.env; .. > -> Job.t -> unit
val waitup : < Shell.caps ; Cap.env; .. > -> unit -> unit
