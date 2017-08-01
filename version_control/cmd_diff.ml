(* Copyright 2017 Yoann Padioleau, see copyright.txt *)
open Common

(* similar to Repository.blob_from_path_and_stat *)
let content_from_path_and_stat path stat_info =
  match stat_info.Index.mode with
  | Index.Link ->
    Unix.readlink path
  | Index.Normal | Index.Exec ->
      path |> Common.with_file_in (fun ch ->
        ch |> IO.input_channel |> IO.read_all
      )
  | Index.Gitlink -> failwith "submodule not supported"

(* less: could factorize with Diff_tree.changes_tree_vs_tree? would need
 * to generate flat list of files (but then less opti opportunity
 * in changes_tree_vs_tree when hash for a whole subtree is the same)
 *)
let changes_index_vs_worktree r =
  r.Repository.index |> List.map (fun entry ->
    let old_stat = entry.Index.stats in
    let path = Filename.concat r.Repository.worktree entry.Index.name in
    let new_stat_opt = 
      try Some (Unix.lstat path |> Index.stat_info_of_lstats)
      with Unix.Unix_error _ -> None
    in
    match new_stat_opt with
    | None -> 
      [Change.Del { Change.path = entry.Index.name;
                    mode = old_stat.Index.mode;
                    content = lazy (Repository.read_blob r entry.Index.id);
                  }]
    | Some new_stat ->
      (match () with
      (* useful opti? *)
      | _ when new_stat.Index.mtime = old_stat.Index.mtime -> []
      (* a change of mode is converted in a del/add *)
      | _ when new_stat.Index.mode <> old_stat.Index.mode ->
        [Change.Del { Change.path = entry.Index.name;
                      mode = old_stat.Index.mode;
                      content = lazy (Repository.read_blob r entry.Index.id)};
         Change.Add { Change.path = entry.Index.name;
                      mode = new_stat.Index.mode;
                      content = lazy (content_from_path_and_stat path new_stat)}
          ]
      | _ -> 
        [Change.Modify (
          { Change.path = entry.Index.name;
            mode = old_stat.Index.mode;
            content = lazy (Repository.read_blob r entry.Index.id) },
          { Change.path = entry.Index.name;
            mode = new_stat.Index.mode;
            content = lazy (content_from_path_and_stat path new_stat) }
        )]
      )
  ) |> List.flatten

let diff_index_vs_worktree r =
  let changes = changes_index_vs_worktree r in
  changes |> List.iter Diff_unified.show_change

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
