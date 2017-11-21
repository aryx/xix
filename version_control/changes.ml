(*s: version_control/changes.ml *)
(*s: copyright ocamlgit *)
(* Copyright 2017 Yoann Padioleau, see copyright.txt *)
(*e: copyright ocamlgit *)
open Common

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* 
 * todo: 
 *  - rename detection
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(*s: function Changes.skip_tree_and_adjust_path *)
let skip_tree_and_adjust_path read_blob dirpath entry_opt =
  match entry_opt with
  | Some { Tree.perm = Tree.Dir } -> None
  | Some { Tree.perm = Tree.Commit } -> failwith "submodule not supported"
  | Some x -> Some ({ Change.
    path = Filename.concat dirpath x.Tree.name;
    mode = Index.mode_of_perm x.Tree.perm;
    
    (* todo: do that later? once know we will return a change with this entry?
     * make it lazy?
     *)
    content = lazy (read_blob x.Tree.id);
  }, x.Tree.id)
  | None -> None
(*e: function Changes.skip_tree_and_adjust_path *)

(*s: function Changes.content_from_path_and_stat_index *)
(* similar to Repository.content_from_path_and_unix_stat *)
let content_from_path_and_stat_index path stat_info =
  match stat_info.Index.mode with
  | Index.Link ->
    Unix.readlink path
  | Index.Normal | Index.Exec ->
      path |> Common.with_file_in (fun ch ->
        ch |> IO.input_channel |> IO.read_all
      )
  (*s: [[Changes.content_from_path_and_stat_index()]] match mode cases *)
  | Index.Gitlink -> failwith "submodule not supported"
  (*e: [[Changes.content_from_path_and_stat_index()]] match mode cases *)
(*e: function Changes.content_from_path_and_stat_index *)

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

(*s: function Changes.changes_tree_vs_tree *)
(* see also Cmd_diff.changes_index_vs_worktree
 *     and  Cmd_status.changes_index_vs_HEAD
 *)
let changes_tree_vs_tree read_tree read_blob tree1 tree2 =
  let changes = ref [] in
  let add x = Common.push x changes in
  Tree.walk_trees read_tree "" (fun dirpath entry1_opt entry2_opt ->
    (* if entries are directories, then we would be called again
     * with their individual files, so safe to skip the dir entries.
     *)
    let entry1_opt = skip_tree_and_adjust_path read_blob dirpath entry1_opt in
    let entry2_opt = skip_tree_and_adjust_path read_blob dirpath entry2_opt in
    
    match entry1_opt, entry2_opt with
    | None, None -> ()
    | Some (a, asha), Some (b, bsha) ->
      (match () with
      (* file type changed reported as delete/add (meh) *)
      | _ when a.Change.mode <> b.Change.mode ->
        add (Change.Del a);
        add (Change.Add b);
      | _ when asha <> bsha ->
        add (Change.Modify (a, b))
      | _ -> ()
      )
    | Some (a,_), None -> add (Change.Del a)
    | None, Some (b,_) -> add (Change.Add b)
  ) tree1 tree2 ;
  List.rev !changes
(*e: function Changes.changes_tree_vs_tree *)

(*s: function Changes.changes_worktree_vs_index *)
(* less: could factorize with Diff_tree.changes_tree_vs_tree? would need
 * to generate flat list of files (but then less opti opportunity
 * in changes_tree_vs_tree when hash for a whole subtree is the same)
 * and then just do set differences to compute new, deleted, and
 * for changes just look intersection and check if same content.
 *)
let changes_worktree_vs_index read_blob worktree index =
  index |> List.map (fun entry ->
    let old_stat = entry.Index.stats in
    let path = Filename.concat worktree entry.Index.path in
    let new_stat_opt = 
      try Some (Unix.lstat path |> Index.stat_info_of_lstats)
      with Unix.Unix_error _ -> None
    in
    match new_stat_opt with
    | None -> 
      [Change.Del { Change.path = entry.Index.path;
                    mode = old_stat.Index.mode;
                    content = lazy (read_blob entry.Index.id);
                  }]
    | Some new_stat ->
      (match () with
      (* useful opti? *)
      | _ when new_stat.Index.mtime = old_stat.Index.mtime -> []
      (* a change of mode is converted in a del/add *)
      | _ when new_stat.Index.mode <> old_stat.Index.mode ->
        [Change.Del { Change.path = entry.Index.path;
                      mode = old_stat.Index.mode;
                      content = lazy (read_blob entry.Index.id)};
         Change.Add { Change.path = entry.Index.path;
                      mode = new_stat.Index.mode;
                      content = lazy 
                        (content_from_path_and_stat_index path new_stat)}
          ]
      | _ -> 
        [Change.Modify (
          { Change.path = entry.Index.path;
            mode = old_stat.Index.mode;
            content = lazy (read_blob entry.Index.id) },
          { Change.path = entry.Index.path;
            mode = new_stat.Index.mode;
            content = lazy 
              (content_from_path_and_stat_index path new_stat) }
        )]
      )
  ) |> List.flatten
(*e: function Changes.changes_worktree_vs_index *)

(*s: function Changes.changes_index_vs_tree *)
(* some commonalities with Repository.set_worktree_and_index_to_tree *)
let changes_index_vs_tree read_tree index treeid =
  let tree = read_tree treeid in

  let h_in_index_and_head = Hashtbl.create 101 in
  let hindex = 
    index 
    |> List.map (fun entry -> entry.Index.path, entry)
    |> Hashtbl_.of_list
  in
  let changes = ref [] in

  tree |> Tree.walk_tree read_tree "" (fun relpath entry_head ->
    let perm = entry_head.Tree.perm in
    match perm with
    | Tree.Dir -> ()
    | Tree.Commit -> failwith "submodule not yet supported"
    | Tree.Normal | Tree.Exec | Tree.Link ->
      try
        let entry_index = Hashtbl.find hindex relpath in
        Hashtbl.add h_in_index_and_head relpath true;
        (* less: if change mode, then report as del/add *)
        if entry_head.Tree.id <> entry_index.Index.id
        then changes |> Common.push (Change.Modify (
          { Change.path = relpath; 
            mode = Index.mode_of_perm perm; 
            content = lazy (raise (Impossible "not called")); },
          { Change.path = relpath; 
            mode = entry_index.Index.stats.Index.mode;
            content = lazy (raise (Impossible "not called")); }
        ))
      with Not_found ->
        changes |> Common.push (Change.Del { Change.
             path = relpath;
             mode = Index.mode_of_perm perm;
             content = lazy (raise (Impossible "not called"));
                                           });
  );
  index |> List.iter (fun entry ->
    if not (Hashtbl.mem h_in_index_and_head entry.Index.path)
    then changes |> Common.push (Change.Add { Change.
             path = entry.Index.path;
             mode = entry.Index.stats.Index.mode;                                            content = lazy (raise (Impossible "not called")); }
    )
  );
  (* less: sort by path *)
  List.rev !changes
(*e: function Changes.changes_index_vs_tree *)
(*e: version_control/changes.ml *)
