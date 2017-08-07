(* Copyright 2017 Yoann Padioleau, see copyright.txt *)
open Common


let diff_worktree_vs_index r =
  let changes = 
    Changes.changes_worktree_vs_index 
      (Repository.read_blob r)
      r.Repository.worktree 
      r.Repository.index 
  in
  changes |> List.iter Diff_unified.show_change

let cmd = { Cmd.
  name = "diff";
  help = " ";
  options = [];
  f = (fun args ->
    (* todo: allow git rm from different location *)
    let r = Repository.open_ "." in
    match args with
    | [] -> diff_worktree_vs_index r
    | xs -> raise Cmd.ShowUsage
  );
}
