(*s: version_control/diff.ml *)
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
 *  - Heckle diff mentionned in diff3.py
 *    "P. Heckel. ``A technique for isolating differences between files.''
 *     Communications of the ACM, Vol. 21, No. 4, page 264, April 1978."
 * 
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(*s: type Diff.item *)
type item = string
(*e: type Diff.item *)

(*s: type Diff.diff_elem *)
(* similar to change.ml, but for content of the file *)
type diff_elem = 
  | Added of item
  | Deleted of item
  | Equal of item
(*e: type Diff.diff_elem *)

(*s: type Diff.diff *)
type diff = diff_elem list
(*e: type Diff.diff *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(*s: function Diff.split_lines *)
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
(*e: function Diff.split_lines *)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

module SimpleDiff = Diff_simple.Make(String)

(*s: function Diff.diff_buggy *)
(*
SimpleDiff is buggy!
Here is an example of output after an ogit diff (with some debugging 
information):

diff --git a/authors.txt b/authors.txt
["Yoann Padioleau\n"; "\n"; "except \n"; " - lex/ by Xavier Leroy (from ocaml)\n"; " - lib_core/stdlib/ by Xavier Leroy et al. (from ocaml)\n"; " - version_control/sha.ml by Daniel Bunzli (from uuidm)\n"; " - version_control/hexsha.ml by Thomas Gazagnaire (from ocaml-hex)\n"; " - version_control/zlib.ml by Xavier Leroy (from camlzip)\n"; " - version_control/unzip.ml by Nicolas Canasse (from extlib)\n"; " - commons/IO.ml by Nicolas Canasse (from exblib)\n"]
["Yoann Padioleau\n"; "\n"; "xxx\n"; "except \n"; " - lex/ by Xavier Leroy (from ocaml)\n"; " - lib_core/stdlib/ by Xavier Leroy et al. (from ocaml)\n"; " - version_control/sha.ml by Daniel Bunzli (from uuidm)\n"; " - version_control/hexsha.ml by Thomas Gazagnaire (from ocaml-hex)\n"; " - version_control/zlib.ml by Xavier Leroy (from camlzip)\n"; " - version_control/unzip.ml by Nicolas Canasse (from extlib)\n"; " - commons/IO.ml by Nicolas Canasse (from exblib)\n"]
[(()); Tag1 (("Yoann Padioleau\n")); Tag2 (("\n", "xxx\n", "except \n", " - lex/ by Xavier Leroy (from ocaml)\n", " - lib_core/stdlib/ by Xavier Leroy et al. (from ocaml)\n", " - version_control/sha.ml by Daniel Bunzli (from uuidm)\n", " - version_control/hexsha.ml by Thomas Gazagnaire (from ocaml-hex)\n", " - version_control/zlib.ml by Xavier Leroy (from camlzip)\n", " - version_control/unzip.ml by Nicolas Canasse (from extlib)\n", " - commons/IO.ml by Nicolas Canasse (from exblib)\n"))]
+Yoann Padioleau
 
 xxx
 except 
  - lex/ by Xavier Leroy (from ocaml)
  - lib_core/stdlib/ by Xavier Leroy et al. (from ocaml)
  - version_control/sha.ml by Daniel Bunzli (from uuidm)
  - version_control/hexsha.ml by Thomas Gazagnaire (from ocaml-hex)
  - version_control/zlib.ml by Xavier Leroy (from camlzip)
  - version_control/unzip.ml by Nicolas Canasse (from extlib)
  - commons/IO.ml by Nicolas Canasse (from exblib)

-----------
SimpleDiff says the diff is the addition of 'Yoann Padioleau' but it's not!
It's in both content. The diff should be the addition of 'xxx'.
*)

let diff_buggy str1 str2 =
  let xs = split_lines str1 in
  let ys = split_lines str2 in
  pr2_gen xs;
  pr2_gen ys;
  let res = SimpleDiff.get_diff (Array.of_list xs) (Array.of_list ys) in
  pr2_gen res;
  res |> List.map (function
    | SimpleDiff.Equal arr   -> 
      arr |> Array.to_list |> List.map (fun x -> Equal x)
    | SimpleDiff.Deleted arr -> 
      arr |> Array.to_list |> List.map (fun x -> Deleted x)
    | SimpleDiff.Added arr   -> 
      arr |> Array.to_list |> List.map (fun x -> Added x)
  )
(*e: function Diff.diff_buggy *)

module StringDiff = Diff_myers.Make(struct
  type t = string array
  type elem = string
  let get t i = Array.get t i
  let length t = Array.length t
end)

(*s: function Diff.diff *)
(* seems correct *)
let diff str1 str2 =
  let xs = split_lines str1 in
  let ys = split_lines str2 in
  let res = StringDiff.diff (Array.of_list xs) (Array.of_list ys) in
  res |> List.rev |> List.map (function
    | `Common (_, _, s) -> Equal s
    | `Removed (_, s) -> Deleted s
    | `Added (_, s) -> Added s
  )
(*e: function Diff.diff *)
(*e: version_control/diff.ml *)
