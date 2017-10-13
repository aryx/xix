(*s: version_control/diffs.ml *)
(*s: copyright ocamlgit *)
(* Copyright 2017 Yoann Padioleau, see copyright.txt *)
(*e: copyright ocamlgit *)
open Common

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Compute the differences between 2 files line-wise.
 *
 * alternatives:
 *  - Simple diff: https://github.com/gjaldon/simple-diff
 *    (an ocaml port of https://github.com/paulgb/simplediff )
 *    http://collaboration.cmc.ec.gc.ca/science/rpn/biblio/ddj/Website/articles/DDJ/1988/8807/8807c/8807c.htm
 *    simple, but no diff -u support by default
 *    and seems buggy!
 *  - Hunt and Mcilroy UNIX diff, the original diff program
 *  - Myers's diff: https://github.com/leque/ocaml-diff port of
 *    "Eugene Myers, An O(ND) Difference Algorithm and Its Variations, 
 *    Algorithmica Vol. 1 No. 2, pp. 251-266, 1986."
 *    https://stackoverflow.com/questions/42635889/myers-diff-algorithm-vs-hunt-mcilroy-algorithm
 *    Apparently Ukkonen discovered independently the same algorithm
 *  - Bram Cohen's Patience diff: https://github.com/janestreet/patdiff
 *    support also colored output, and word diff, but heavily modularized
 *    https://blog.jcoglan.com/2017/09/19/the-patience-diff-algorithm/
 *    https://blog.jcoglan.com/2017/09/28/implementing-patience-diff/
 *  - Plan9 diff (in plan9/utilities/string/diff/)
 *    not Myers's diff
 *  - GNU diff (in plan9/ape_cmd/diff)
 *    is based on Myers's algorithm
 *  - http://pynash.org/2013/02/26/diff-in-50-lines/ 
 *    (in python, and talk about python difflib)
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

(*s: module Diffs.StringDiff *)
module StringDiff = Diff_myers.Make(struct
  type t = string array
  type elem = string
  let get t i = Array.get t i
  let length t = Array.length t
end)
(*e: module Diffs.StringDiff *)

(*s: function Diffs.diff *)
(* seems correct *)
let diff str1 str2 =
  let xs = split_lines str1 in
  let ys = split_lines str2 in
  Diff_basic.diff (Array.of_list xs) (Array.of_list ys)
(*
  let res = StringDiff.diff (Array.of_list xs) (Array.of_list ys) in
  res |> List.rev |> List.map (function
    | `Common (_, _, s) -> Equal s
    | `Removed (_, s) -> Deleted s
    | `Added (_, s) -> Added s
  )
*)
(*e: function Diffs.diff *)
(*e: version_control/diffs.ml *)
