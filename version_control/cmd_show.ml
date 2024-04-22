(*s: version_control/cmd_show.ml *)
(*s: copyright ocamlgit *)
(* Copyright 2017 Yoann Padioleau, see copyright.txt *)
(*e: copyright ocamlgit *)
open Stdcompat (* for |> *)
open Common

(*s: function [[Cmd_show.show]] *)
let show r objectish =
  let sha, obj = Repository.read_objectish r objectish in
  match obj with
  (*s: [[Cmd_show.show()]] match obj cases *)
  | Objects.Blob x -> 
    Blob.show x
  (*x: [[Cmd_show.show()]] match obj cases *)
  | Objects.Tree x ->
    (* =~ git ls-tree --names-only *)
    pr (spf "tree %s\n" (Hexsha.of_sha sha));
    Tree.show x
  (*x: [[Cmd_show.show()]] match obj cases *)
  | Objects.Commit x -> 
    pr (spf "commit %s" (Hexsha.of_sha sha));
    Commit.show x;
    let tree2 = Repository.read_tree r x.Commit.tree in
    let tree1 = 
      try 
        let parent1 = Repository.read_commit r (List.hd x.Commit.parents) in
        Repository.read_tree r parent1.Commit.tree 
      with Failure _ ->
      (* no parent *)
        []
    in
    let changes = 
      Changes.changes_tree_vs_tree 
        (Repository.read_tree r) 
        (Repository.read_blob r)
        tree1 tree2 
    in
    changes |> List.iter Diff_unified.show_change
  (*x: [[Cmd_show.show()]] match obj cases *)
  (*e: [[Cmd_show.show()]] match obj cases *)
(*e: function [[Cmd_show.show]] *)

(*s: constant [[Cmd_show.cmd]] *)
let cmd = { Cmd.
  name = "show";
  usage = " <objectish>";
  (* less: --oneline *)
  options = [];
  f = (fun args ->
    let r, _ = Repository.find_root_open_and_adjust_paths [] in
    match args with
    | [] -> show r (Repository.ObjByRef (Refs.Head))
    | xs ->
      xs |> List.iter (fun str ->
        show r (Repository.parse_objectish str)
      )
  );
}
(*e: constant [[Cmd_show.cmd]] *)
(*e: version_control/cmd_show.ml *)
