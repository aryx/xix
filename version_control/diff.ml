(* Copyright 2017 Yoann Padioleau, see copyright.txt *)
open Common

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Compute the differences between 2 files line-wise.
 *
 * alternatives:
 *  - simple diff: https://github.com/gjaldon/simple-diff
 *    (an ocaml port of https://github.com/paulgb/simplediff )
 *    http://collaboration.cmc.ec.gc.ca/science/rpn/biblio/ddj/Website/articles/DDJ/1988/8807/8807c/8807c.htm
 *    simple, but no diff -u support by default
 *    and seems buggy!
 *  - myers: https://github.com/leque/ocaml-diff port of
 *    "Eugene Myers, An O(ND) Difference Algorithm and Its Variations, 
 *    Algorithmica Vol. 1 No. 2, pp. 251-266, 1986."
 *  - patience diff: https://github.com/janestreet/patdiff
 *  https://stackoverflow.com/questions/42635889/myers-diff-algorithm-vs-hunt-mcilroy-algorithm
 *    support also colored output, and word diff, but heavily modularized
 *  - plan9 diff (in plan9/utilities/string/diff/)
 *    not myers's diff
 *  - gnu diff (in plan9/ape_cmd/diff)
 *    use myers?
 *  - http://pynash.org/2013/02/26/diff-in-50-lines/ 
 *    (in python, and talk about python difflib)
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type item = string

type diff_elem = 
  | Deleted of item list
  | Added of item list
  | Equal of item list

type diff = diff_elem list

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
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

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

module SimpleDiff = Simple_diff.Make(String)

let diff str1 str2 =
  let xs = split_lines str1 in
  let ys = split_lines str2 in
  let res = SimpleDiff.get_diff (Array.of_list xs) (Array.of_list ys) in
  res |> List.map (function
    | SimpleDiff.Equal arr   -> Equal (Array.to_list arr)
    | SimpleDiff.Deleted arr -> Deleted (Array.to_list arr)
    | SimpleDiff.Added arr   -> Added (Array.to_list arr)
  )
