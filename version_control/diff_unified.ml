(* Copyright 2017 Yoann Padioleau, see copyright.txt *)
open Common

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Show differences between 2 files.
 * 
 * alternatives:
 *  - parse and display diffs:
 *    http://zoggy.github.io/ocamldiff/
 *  - call diff -u (as I did in pfff) directly via Sys.command
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

let show_change read_blob change =
  (* less: if mode is gitlink? *)
  let (old_path, old_content), (new_path, new_content) = 
    match change with
    | Change.Add entry ->
      ("dev/null", ""), 
      ("b/" ^ entry.Tree.name, read_blob entry.Tree.node)
    | Change.Del entry ->
      ("a/" ^ entry.Tree.name, read_blob entry.Tree.node), 
      ("dev/null", "")
    | Change.Modify (entry1, entry2) ->
      ("a/" ^ entry1.Tree.name, read_blob entry1.Tree.node), 
      ("b/" ^ entry2.Tree.name, read_blob entry2.Tree.node)
  in
  pr (spf "diff --git %s %s" old_path new_path);
  (* less: display change of modes *)
  ()
