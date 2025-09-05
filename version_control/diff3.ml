open Stdcompat (* for |> *)
open Common
open Ord.Operators

module D = Diff

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Merge of files (aka diff3).
 *
 * Pierce et al. formalized the diff3 algorithm in 
 * "A formal Investigation of Diff3" - FSTTCS 2007
 * Foundations of Software Technology and Theoretical Computer Science
 * 
 * alternatives:
 *  - diff3.c by Randy Smith (original author of the diff3 algorithm)
 *  - Text::Diff3 in Perl and its Python port diff3.py
 *    short, but hard to understand
 *  - https://blog.jcoglan.com/2017/05/08/merging-with-diff3/
 *    based on Pierce's paper
 *  - Diff3 in Javascript
 *    http://homepages.kcbbs.gen.nz/tonyg/projects/synchrotron.html
 *    https://github.com/tonyg/synchrotron/blob/master/diff.js
 *    also based on Pierce's paper
 * 
 * The code below uses 0-based indexing of arrays (OCaml arrays uses that)
 * which is different than the 1-based indexing used in the paper
 * referenced above.
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* Final result (see Pierce et al. paper *)
type chunk =
  | Stable of Diff.item (* less: could be a list here *)
  | ChangedA of Diff.item list (* Orig *) * Diff.item list (* A *)
  | ChangedB of Diff.item list (* Orig *) * Diff.item list (* B *)
  | FalseConflict of Diff.item list
  | TrueConflict of 
      Diff.item list (* Orig *) * Diff.item list (*A*) * Diff.item list (*B*)

(* intermediate types *)
type _matching_lines = 
    (int (* line# in original *) * 
     int (* corresponding line# in modified file *))
    list

type _identical_lines =
    (int (* line# in original *) * int (* line# in A *) * int (* line# in B *))
    list

type chunk_basic = 
  | Same of int
  | Changed of (int * int) * (int * int) * (int * int)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let matching_lines_of_diff diff =
  let rec aux old_line new_line xs =
    match xs with
    | [] -> []
    | x::xs -> 
      match x with
      | D.Equal _ -> 
        (old_line, new_line)::aux (old_line + 1) (new_line + 1) xs
      | D.Added _ ->
        aux old_line (new_line + 1) xs
      | D.Deleted _ ->
        aux (old_line + 1) new_line xs
  in
  aux 0 0 diff

(* unchanged in both diffs *)
let rec identical_lines xs ys =
  match xs, ys with
  | [], [] -> []
  | _x::_xs, [] -> []
  | [], _y::_ys -> []
  | (o1,a)::xs, (o2,b)::ys ->
    (match o1 <=> o2 with
    | Equal -> (o1, a, b)::identical_lines xs ys
    | Less -> identical_lines xs ((o2,b)::ys)
    | Greater -> identical_lines ((o1,a)::xs) ys
    )

(* aligning *)
let basic_chunks (leno, lena, lenb) identical =
  let rec aux lo la lb xs =
  match xs with
  | [] -> 
    if lo < leno || la < lenb || lb < lenb
    then [Changed ((lo, leno),(la, lena), (lb, lenb))]
    else []
  | (o, a, b)::xs ->
    if a = la && b = lb
    then (Same o)::aux (o+1) (a+1) (b+1) xs
    else (Changed ((lo, o), (la, a), (lb, b)))::(Same o)::
      aux (o+1) (a+1) (b+1) xs
  in
  aux 0 0 0 identical

let rec span_range arr l1 l2=
  if l1 = l2
  then []
  else arr.(l1)::span_range arr (l1+1) l2

let final_chunks oxs axs bxs chunks =
  let rec aux xs =
  match xs with
  | [] -> []
  | x::xs ->
    let chunk = 
      (match x with
      | Same o -> Stable (oxs.(o))
      | Changed ((o1, o2),(a1, a2), (b1, b2)) ->
        let oh = span_range oxs o1 o2 in
        let ah = span_range axs a1 a2 in
        let bh = span_range bxs b1 b2 in
        match oh = ah, oh = bh with
        | false, true ->
          ChangedA (oh, ah)
        | true, false ->
          ChangedB (oh, bh)
        | true, true -> raise (Impossible "should be Same then")
        | false, false ->
          if ah = bh
          then FalseConflict ah
          else TrueConflict (oh, ah, bh)
      )
    in
    chunk::aux xs
  in
  aux chunks

(*****************************************************************************)
(* Output *)
(*****************************************************************************)

(* less: have an option where show also the original inside a ||||||| *)
let merge filea fileb chunks =
  let rec aux xs =
    match xs with
    | [] -> []
    | x::xs ->
      (match x with
      | Stable s -> [s]
      | ChangedA (_, items)
      | ChangedB (_, items)
      | FalseConflict (items) ->
        items
      | TrueConflict (_orig, a, b) ->
        (* less: fix a and b if no trailing \n *)
        (* see -m and https://www.gnu.org/software/diffutils/manual/html_node/Merging-Incomplete-Lines.html#Merging-Incomplete-Lines *)
        [(spf "<<<<<<< %s\n" filea);] @
         a @
        ["=======\n"] @
         b @
        [(spf ">>>>>>> %s\n" fileb);]
      ):: aux xs
  in
  let xxs = aux chunks in
  let all_lines = List.flatten xxs in
  String.concat "" all_lines

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let diff3 str_origin str_a str_b =
  let oxs = Diffs.split_lines str_origin |> Array.of_list in
  let axs = Diffs.split_lines str_a |> Array.of_list in
  let bxs = Diffs.split_lines str_b |> Array.of_list in

  (* less: could go through Diffs.diff_array instead of hardcoded Diff_myers?*)
  let diff_oa = Diff_myers.diff oxs axs in
  let diff_ob = Diff_myers.diff oxs bxs in

  let match_oa = matching_lines_of_diff diff_oa in
  let match_ob = matching_lines_of_diff diff_ob in
  let same_all = identical_lines match_oa match_ob in
  let basic_chunks = 
    basic_chunks
      ((Array.length oxs),
       (Array.length axs),
       (Array.length bxs)
      )
      same_all in
  final_chunks oxs axs bxs basic_chunks
