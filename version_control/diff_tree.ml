(* Copyright 2017 Yoann Padioleau, see copyright.txt *)
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
    content = lazy (read_blob x.Tree.node);
  }, x.Tree.node)
  | None -> None

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

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
      (* file type changed reported as delete/add *)
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
