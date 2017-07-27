(* Copyright 2017 Yoann Padioleau, see copyright.txt *)
open Common

let diff_index_vs_worktree r =
  r.Repository.index |> List.iter (fun entry ->
    let old_stat = entry.Index.stats in
    let path = Filename.concat r.Repository.worktree entry.Index.name in
    let new_stat = 
      try 
        Some (Unix.lstat path |> Index.stat_info_of_lstats)
      with Unix.Unix_error _ -> None
    in
    
    raise Todo
  )


let cmd = { Cmd.
  name = "diff";
  help = " ";
  options = [];
  f = (fun args ->
    (* todo: allow git rm from different location *)
    let r = Repository.open_ "." in
    match args with
    | [] -> diff_index_vs_worktree r
    | xs -> raise Cmd.ShowUsage
  );
}
