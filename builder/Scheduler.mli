(*s: Scheduler.mli *)
(*s: signature [[Scheduler.nrunning]] *)
val nrunning : int ref
(*e: signature [[Scheduler.nrunning]] *)

(*s: signature [[Scheduler.run]] *)
(* need Cap.env for NPROC *)
val run : < Shell.caps ; Cap.env; .. > -> Job.t -> unit
(*e: signature [[Scheduler.run]] *)
(*s: signature [[Scheduler.waitup]] *)
val waitup : < Shell.caps ; Cap.env; .. > -> unit -> unit
(*e: signature [[Scheduler.waitup]] *)
(*e: Scheduler.mli *)
