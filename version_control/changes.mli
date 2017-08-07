
(* for git show commit *)
val changes_tree_vs_tree: 
  (Tree.hash -> Tree.t) ->
  (Blob.hash -> Change.content) ->
  Tree.t -> Tree.t -> Change.t list

(* for git diff and git status *)
val changes_worktree_vs_index:
  (Blob.hash -> Change.content) ->
  Common.filename -> Index.t -> Change.t list

(* for git status (to compare index vs HEAD) *)
val changes_index_vs_tree:
  (Tree.hash -> Tree.t) ->
  Index.t -> Tree.hash -> Change.t list
