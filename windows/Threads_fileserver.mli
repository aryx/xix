(*s: Threads_fileserver.mli *)
(*s: signature [[Threads_fileserver.thread]] *)
(* 
  will serve all the virtual devices for new windows/processes created by rio.
 *)
val thread: Fileserver.t -> unit
(*e: signature [[Threads_fileserver.thread]] *)
(*e: Threads_fileserver.mli *)
