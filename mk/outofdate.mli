
(* May call internally Scheduler.run to schedule jobs.
 * It will also modify by side effect the graph to set to 
 * Made or BeingMade some nodes.
 *)
val work: 
  Graph.graph -> bool ref -> unit
