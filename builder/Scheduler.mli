(*s: Scheduler.mli *)
(*s: signature [[Scheduler.nrunning]] *)
val nrunning : int ref
(*e: signature [[Scheduler.nrunning]] *)

(*s: signature [[Scheduler.run]] *)
(* need Cap.env for NPROC *)
val run : < Shell.caps ; Cap.env; .. > -> Job.t -> unit
(*e: signature [[Scheduler.run]] *)
(*s: signature [[Scheduler.waitup]] *)
(* need Cap.open_out to delete target file if error in recipe process *)
val waitup : < Shell.caps ; Cap.open_out; .. > -> unit -> unit
(*e: signature [[Scheduler.waitup]] *)
(*e: Scheduler.mli *)
