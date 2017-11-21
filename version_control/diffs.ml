(*s: version_control/diffs.ml *)
(*s: copyright ocamlgit *)
(* Copyright 2017 Yoann Padioleau, see copyright.txt *)
(*e: copyright ocamlgit *)
open Common

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Compute the differences between two files line-wise.
 *
 * alternatives:
 *  - Basic diff using simple edit distance algorithm
 *    but O(nm) time and space complexity (space especially is bad)
 *  - Hirshberg invented a linear space optimization of basic diff
 *  - Hunt and Mcilroy UNIX diff, the original diff program
 *    rely on pair of lines which are the same in both files
 *    (but if the two files contain blank lines, then very bad complexity)
 *  - Myers's diff: https://github.com/leque/ocaml-diff port of
 *    "Eugene Myers, An O(ND) Difference Algorithm and Its Variations, 
 *    Algorithmica Vol. 1 No. 2, pp. 251-266, 1986."
 *    https://stackoverflow.com/questions/42635889/myers-diff-algorithm-vs-hunt-mcilroy-algorithm
 *    Apparently Ukkonen discovered independently the same algorithm
 *    Good if the edit distance is small, but very bad if both files are
 *    very different.
 *  - Simple diff: https://github.com/gjaldon/simple-diff
 *    (an OCaml port of https://github.com/paulgb/simplediff )
 *    http://collaboration.cmc.ec.gc.ca/science/rpn/biblio/ddj/Website/articles/DDJ/1988/8807/8807c/8807c.htm article explaining but assembly code (wtf)
 *    based on Ratcliff/Obershelp pattern-matching algorithm
 *    seems simple, but not optimal (but "looks right" to people)
 *    but ocaml version seems buggy!
 *  - Bram Cohen's Patience diff: https://github.com/janestreet/patdiff
 *    support also colored output, and word diff, but heavily modularized
 *    https://blog.jcoglan.com/2017/09/19/the-patience-diff-algorithm/
 *    https://blog.jcoglan.com/2017/09/28/implementing-patience-diff/
 *  - Plan9 diff (in plan9/utilities/string/diff/)
 *    not Myers's diff
 *  - GNU diff (in plan9/ape_cmd/diff)
 *    is based on Myers's algorithm
 *  - http://pynash.org/2013/02/26/diff-in-50-lines/ 
 *    implement Mcilroy in Python (also talk about Python difflib)
 *  - Heckle diff mentionned in diff3.py
 *    "P. Heckel. ``A technique for isolating differences between files.''
 *     Communications of the ACM, Vol. 21, No. 4, page 264, April 1978."
 * 
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
(*s: function Diffs.split_lines *)
let split_lines str =
  (* alt: let xs = Str.full_split (Str.regexp "\n") str in *)
  let rec aux start = 
    try
      let idx = String.index_from str start '\n' in
      let line = String.sub str start (idx - start + 1) in
      line::aux (idx + 1)
    with Not_found ->
      if start = String.length str
      then []
      else [String.sub str start (String.length str - start)]
  in
  aux 0
(*e: function Diffs.split_lines *)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

(*s: function Diffs.diff *)
let diff str1 str2 =
  let xs = split_lines str1 in
  let ys = split_lines str2 in
  Diff_myers.diff (Array.of_list xs) (Array.of_list ys)
(*e: function Diffs.diff *)
(*e: version_control/diffs.ml *)
