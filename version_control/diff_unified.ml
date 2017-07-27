(* Copyright 2017 Yoann Padioleau, see copyright.txt *)
open Common

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Show differences between 2 files.
 *
 * Short explanation of unified format:
 *  - https://www.gnu.org/software/diffutils/manual/html_node/Detailed-Unified.html
 *  - http://www.artima.com/weblogs/viewpost.jsp?thread=164293  
 * 
 * alternatives:
 *  - ocamldiff: https://github.com/zoggy/ocamldiff 
 *    parse and display unified diffs
 *  - unidiff: https://github.com/gildor478/ocaml-unidiff
 *    parse 
 *  - call diff -u (as I did in pfff) directly via Sys.command
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let show_unified_diff old_content new_content =
  let diffs = Diff.diff old_content new_content in
  (* todo: contextual diff! *)
  diffs |> List.iter (function
    | Diff.Equal xs -> 
      xs |> List.iter (fun s -> print_string (" " ^ s))
    | Diff.Deleted xs -> 
      xs |> List.iter (fun s -> print_string ("-" ^ s))
    | Diff.Added xs -> 
      xs |> List.iter (fun s -> print_string ("+" ^ s))
  )


(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

let show_change change =
  (* less: if mode is gitlink? *)
  let (old_path, old_content), (new_path, new_content) = 
    match change with
    | Change.Add entry ->
      ("dev/null", ""), 
      ("b/" ^ entry.Change.path, entry.Change.content)
    | Change.Del entry ->
      ("a/" ^ entry.Change.path, entry.Change.content), 
      ("dev/null", "")
    | Change.Modify (entry1, entry2) ->
      ("a/" ^ entry1.Change.path, entry1.Change.content), 
      ("b/" ^ entry2.Change.path, entry2.Change.content)
  in
  pr (spf "diff --git %s %s" old_path new_path);
  (* less: display change of modes *)
  show_unified_diff old_content new_content
