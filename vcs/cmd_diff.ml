(*s: version_control/cmd_diff.ml *)
(*s: copyright ocamlgit *)
(* Copyright 2017 Yoann Padioleau, see copyright.txt *)
(*e: copyright ocamlgit *)

(*s: function [[Cmd_diff.diff_worktree_vs_index]] *)
let diff_worktree_vs_index caps r =
  let changes = 
    Changes.changes_worktree_vs_index caps
      (Repository.read_blob r)
      r.Repository.worktree 
      r.Repository.index 
  in
  changes |> List.iter (Diff_unified.show_change caps)
(*e: function [[Cmd_diff.diff_worktree_vs_index]] *)

(*s: constant [[Cmd_diff.cmd]] *)
let cmd = { Cmd_.
  name = "diff";
  usage = " ";
  options = [];
  f = (fun caps args ->
    let r, _ = Repository.find_root_open_and_adjust_paths caps [] in
    match args with
    | [] -> diff_worktree_vs_index caps r
    | _xs -> raise Cmd_.ShowUsage
  );
}
(*e: constant [[Cmd_diff.cmd]] *)
(*e: version_control/cmd_diff.ml *)
