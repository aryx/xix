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

let skip_tree_and_adjust_path dirpath entry_opt =
  match entry_opt with
  | Some { Tree.perm = Tree.Dir } -> None
  | Some { Tree.perm = Tree.Commit } -> failwith "submodule not supported"
  | Some x -> Some { Change.
    path = Filename.concat dirpath x.Tree.name;
    mode = Index.mode_of_perm x.Tree.perm;
    content = x.Tree.node;
  }
  | None -> None

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

let tree_changes read_tree tree1 tree2 =
  let changes = ref [] in
  let add x = Common.push x changes in
  Tree.walk_trees read_tree "" (fun dirpath entry1_opt entry2_opt ->
    (* if entries are directories, then we would be called again
     * with their individual files, so safe to skip the dir entries.
     *)
    let entry1_opt = skip_tree_and_adjust_path dirpath entry1_opt in
    let entry2_opt = skip_tree_and_adjust_path dirpath entry2_opt in
    
    match entry1_opt, entry2_opt with
    | a, b when a = b -> () (* Identical *)
    | Some a, Some b ->
      (* file type changed reported as delete/add *)
      if a.Change.mode <> b.Change.mode 
      then begin 
        add (Change.Del a);
        add (Change.Add b);
      end
      else add (Change.Modify (a, b))
    | Some a, None -> add (Change.Del a)
    | None, Some b -> add (Change.Add b)
    | None, None -> raise (Impossible "cover by a = b above")
  ) tree1 tree2 ;
  List.rev !changes
