(*s: version_control/changes.mli *)

(*s: signature [[Changes.changes_tree_vs_tree]] *)
(* for git show commit *)
val changes_tree_vs_tree: 
  (Tree.hash -> Tree.t) ->
  (Blob.hash -> Change.content) ->
  Tree.t -> Tree.t -> Change.t list
(*e: signature [[Changes.changes_tree_vs_tree]] *)

(*s: signature [[Changes.changes_worktree_vs_index]] *)
(* for git diff and git status *)
val changes_worktree_vs_index:
  (Blob.hash -> Change.content) ->
  Common.filename -> Index.t -> Change.t list
(*e: signature [[Changes.changes_worktree_vs_index]] *)

(*s: signature [[Changes.changes_index_vs_tree]] *)
(* for git status (to compare index vs HEAD) *)
val changes_index_vs_tree:
  (Tree.hash -> Tree.t) ->
  Index.t -> Tree.hash -> Change.t list
(*e: signature [[Changes.changes_index_vs_tree]] *)
(*e: version_control/changes.mli *)
