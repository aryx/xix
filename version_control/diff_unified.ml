(*s: version_control/diff_unified.ml *)
(*s: copyright ocamlgit *)
(* Copyright 2017 Yoann Padioleau, see copyright.txt *)
(*e: copyright ocamlgit *)
open Stdcompat (* for bytes *)
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

(*s: function [[Diff_unified.print]] *)
let print = function
  | Diff.Equal s -> 
    print_string (" " ^ s)
  | Diff.Deleted s -> 
    print_string ("-" ^ s)
  | Diff.Added s -> 
    print_string ("+" ^ s)
(*e: function [[Diff_unified.print]] *)

(*s: function [[Diff_unified.print_header]] *)
let print_header nctx_before nold nnew =
  (* todo: should print size of hunk also here, but then
   * need to wait we finished processing this hunk
   *)
  print_string (spf "@@ -%d, +%d, @@\n"
                  (nold - nctx_before) (nnew - nctx_before))
(*e: function [[Diff_unified.print_header]] *)

(*s: constant [[Diff_unified.nContext]] *)
let nContext = 3
(*e: constant [[Diff_unified.nContext]] *)

(*s: function [[Diff_unified.show_unified_diff]] *)
let show_unified_diff diffs =
  (* naive: no contextual:  diffs |> List.iter print *)
  let rec aux context_lines nctx_before nctx_after nold nnew diffs =
    match diffs with
    (* todo: say if 'No newline at end of file' *)
    | [] -> ()
    | x::xs ->
      (match x with
      | Diff.Equal _s ->
        (match () with
        | _ when nctx_after > 0 ->
          print x;
          aux [] 0 (nctx_after - 1) (nold + 1) (nnew + 1) xs
        | _ when nctx_before < nContext ->
          aux (x::context_lines) (nctx_before + 1) 0 (nold + 1) (nnew + 1) xs
        | _ when nctx_before = nContext ->
          let new_context_lines = List_.take nContext (x::context_lines) in
          aux new_context_lines nContext 0 (nold + 1) (nnew + 1) xs
        | _ -> raise (Impossible "")
        )
      | Diff.Deleted _s  ->
        let prevs = List_.take nctx_before context_lines |> List.rev in
        if prevs <> [] then print_header nctx_before nold nnew;
        prevs |> List.iter print;
        print x;
        aux [] 0 nContext (nold + 1) (nnew) xs
      | Diff.Added _s ->
        let prevs = List_.take nctx_before context_lines |> List.rev in
        if prevs <> [] then print_header nctx_before nold nnew;
        prevs |> List.iter print;
        print x;
        aux [] 0 nContext (nold) (nnew+1) xs
      )
  in
  aux [] 0 0 1 1 diffs
(*e: function [[Diff_unified.show_unified_diff]] *)


(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

(*s: function [[Diff_unified.show_change]] *)
let show_change change =
  (* less: if mode is gitlink? *)
  let (old_path, old_content), (new_path, new_content) = 
    match change with
    | Change.Add entry ->
      ("dev/null", lazy ""), 
      ("b/" ^ entry.Change.path, entry.Change.content)
    | Change.Del entry ->
      ("a/" ^ entry.Change.path, entry.Change.content), 
      ("dev/null", lazy "")
    | Change.Modify (entry1, entry2) ->
      ("a/" ^ entry1.Change.path, entry1.Change.content), 
      ("b/" ^ entry2.Change.path, entry2.Change.content)
  in
  let diffs = Diffs.diff (Lazy.force old_content) (Lazy.force new_content) in
  if not (diffs |> List.for_all (function Diff.Equal _ -> true | _ -> false))
  then begin
    pr (spf "diff --git %s %s" old_path new_path);
    (* less: display change of modes *)
    show_unified_diff diffs
  end
(*e: function [[Diff_unified.show_change]] *)
(*e: version_control/diff_unified.ml *)
