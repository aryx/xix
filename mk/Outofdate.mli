(* work() may call internally Scheduler.run() to schedule jobs.
 * It will also modify by side effect the graph to set to 
 * Made or BeingMade some nodes.
 * May raise Failure ("don't know how to make xxx" | "no recipe to make xxx")
 *)
val work :
  < Shell.caps ; .. > ->
  Env.t ->
  Graph.node ->
  bool ref (* OUT parameter, did anything *) ->
  unit
