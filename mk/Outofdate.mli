
(* work() may call internally Scheduler.run() to schedule jobs.
 * It will also modify by side effect the graph to set to 
 * Made or BeingMade some nodes.
 *)
val work: 
  < Cap.exec; .. > ->
  Env.t -> Graph.node -> bool ref (* OUT parameter, did anything *) -> unit
