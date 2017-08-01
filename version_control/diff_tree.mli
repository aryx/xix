
val changes_tree_vs_tree: 
  (Tree.hash -> Tree.t) ->
  (Blob.hash -> Change.content) ->
  Tree.t -> Tree.t -> Change.t list
