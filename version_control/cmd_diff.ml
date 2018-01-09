(*s: version_control/cmd_diff.ml *)
(*s: copyright ocamlgit *)
(* Copyright 2017 Yoann Padioleau, see copyright.txt *)
(*e: copyright ocamlgit *)
open Common

(*s: function Cmd_diff.diff_worktree_vs_index *)
let diff_worktree_vs_index r =
  let changes = 
    Changes.changes_worktree_vs_index 
      (Repository.read_blob r)
      r.Repository.worktree 
      r.Repository.index 
  in
  changes |> List.iter Diff_unified.show_change
(*e: function Cmd_diff.diff_worktree_vs_index *)

(*s: constant Cmd_diff.cmd *)
let cmd = { Cmd.
  name = "diff";
  usage = " ";
  options = [];
  f = (fun args ->
    let r, _ = Repository.find_root_open_and_adjust_paths [] in
    match args with
    | [] -> diff_worktree_vs_index r
    | xs -> raise Cmd.ShowUsage
  );
}
(*e: constant Cmd_diff.cmd *)
(*e: version_control/cmd_diff.ml *)
