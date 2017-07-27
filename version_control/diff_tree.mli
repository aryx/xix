
val tree_changes: 
  (Tree.hash -> Tree.t) ->
  (Blob.hash -> Change.content) ->
  Tree.t -> Tree.t -> Change.t list
