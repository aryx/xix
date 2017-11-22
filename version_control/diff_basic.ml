(*s: version_control/diff_basic.ml *)
open Common

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Basic Diff algorithm based on the computation of the edit distance between
 * two strings (also known as the Longuest Common Subsequence (LCS) problem).
 * 
 * reference: 
 * "Algorithms on Strings, Trees, and Sequences" by Dan Gusfield, P217.
 * 
 * For two strings S1 and S2, D(i,j) is defined to be the edit distance 
 * of S1[1..i] to S2[1..j].
 * So the edit distance of S1 (of length n) and S2 (of length m) is D(n,m).
 * 
 * Dynamic programming technique
 * base: 
 *  D(i,0) = i  for all i 
 *   because to go from S1[1..i] to 0 characters of S2 you have to 
 *   delete all the characters from S1[1..i]
 *  D(0,j) = j  for all j 
 *    because j characters must be inserted
 * recurrence:
 *  D(i,j) = min([D(i-1, j)+1, 
 *                D(i, j-1)+1, 
 *                D(i-1, j-1) + t(i,j)])
 *   where t(i,j) is equal to 1 if S1(i) != S2(j) and  0 if equal
 * 
 *  Intuition = there are 4 possible actions:
 *    deletion, insertion, substitution, or match
 *  so Lemma =
 *   D(i,j) must be one of the three
 *     D(i, j-1) + 1
 *     D(i-1, j)+1 
 *     D(i-1, j-1) + 
 *       t(i,j) 
 * 
 * history: done in summer 2007 for Julia Lawall
 * 
 * complexity: O(nm) in time and space, so if you want to diff two
 * sequences of more than 10 000 elts, it will requires a matrix with
 * 100 M and 100 M operations, so this is not very scalable 
 * (see Myers's algorithm used in GNU diff for a more scalable algorithm).
 *)

(*****************************************************************************)
(* Algorithm *)
(*****************************************************************************)

(*s: function Diff_basic.matrix_distance *)
let matrix_distance arr1 arr2 = 
  let n = Array.length arr1 in
  let m = Array.length arr2 in 
  (* this can be big ... *)
  let mat = Array.make_matrix (n+1) (m+1) 0 in
  let t i j = 
    if Array.get arr1 (i-1) = Array.get arr2 (j-1)
    then 0
    else 1 
  in
  let min3 a b c = min (min a b) c in

  begin
    for i = 0 to n do
      mat.(i).(0) <- i
    done;
    for j = 0 to m do
      mat.(0).(j) <- j;
    done;
    (* this can be long ... *)
    for i = 1 to n do
      for j = 1 to m do
        mat.(i).(j) <- 
          min3 (mat.(i).(j-1) + 1) 
               (mat.(i-1).(j) + 1) 
               (mat.(i-1).(j-1) + t i j)
      done
    done;
    mat
  end
(*e: function Diff_basic.matrix_distance *)

(*s: function Diff_basic.traceback_transcript *)
(* extract the traceback from the matrice (Gusfield P221) *)
let traceback_transcript arr1 arr2 mat =
  let n = Array.length arr1 in
  let m = Array.length arr2 in 
  let get_orig_arr arr i = arr.(i-1) in
  (* you need Figure 11.3 P222 of Gusfield book to understand the code below *)
  let rec aux i j =
    let x = mat.(i).(j) in
    match () with
    | _ when i = 0 && j = 0 -> []
    | _ when i = 0 -> (Diff.Added (get_orig_arr arr2 j))::aux (i) (j-1)
    | _ when j = 0 -> (Diff.Deleted (get_orig_arr arr1 i))::aux (i-1) (j)
    | _ when x = mat.(i-1).(j) + 1 -> 
      (Diff.Deleted (get_orig_arr arr1 i))::aux (i-1) (j)
    | _ when x = mat.(i).(j-1) + 1 -> 
      (Diff.Added (get_orig_arr arr2 j))::aux (i) (j-1)
    | _ -> 
      if x = mat.(i-1).(j-1)
      then Diff.Equal (get_orig_arr arr1 (i))::aux (i-1) (j-1)
      else (Diff.Deleted (get_orig_arr arr1 i))::
           (Diff.Added (get_orig_arr arr2 j))::
           aux (i-1) (j-1)
  in
  aux n m
(*e: function Diff_basic.traceback_transcript *)

(*****************************************************************************)
(* Optimization *)
(*****************************************************************************)
(* Actually this is not a big win. On files of 20 000 LOC, you
 * go from 125s to 118s, so the string comparison in matrix_distance()
 * is not the bottleneck.
 * Actually in native mode you go from 33s to 35s so this is even
 * slowing things down, weird.
 *)
let hash_strings arr1 arr2 =
  let cnt = ref 0 in
  let h = Hashtbl.create (Array.length arr1 / 3) in
  Array.concat [arr1; arr2] |> Array.iter (fun s ->
    if not (Hashtbl.mem h s)
    then begin
      Hashtbl.add h s !cnt;
      incr cnt;
    end
  );
  let revh = Array.make !cnt ("INCORRECT") in
  h |> Hashtbl.iter (fun k v ->
    revh.(v) <- k
  );
  arr1 |> Array.map (fun s -> Hashtbl.find h s),
  arr2 |> Array.map (fun s -> Hashtbl.find h s),
  revh

(* other optimizations:
 *  - strip common lines in the front and end (McIlroy diff was doing that)
 *    so can reduce significantly the size of the matrix
 *  - Use Hirschberg space opti of storing only the last row.
 *    No need for full matrix to determine the next row (but then
 *    need to be clever to get the traceback).
 *)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)
(*s: function Diff_basic.diff *)
let diff arr1 arr2 =
  (* opti: string to int *)

  let mat = matrix_distance arr1 arr2 in
  let trace = traceback_transcript arr1 arr2 mat in
  List.rev trace

  (* opti: get back string from int *)
(*
  let arr1, arr2, revh = hash_strings arr1 arr2 in
  |> List.map (function 
      | Diff.Added i   -> Diff.Added   (revh.(i))
      | Diff.Deleted i -> Diff.Deleted (revh.(i))
      | Diff.Equal i   -> Diff.Equal   (revh.(i))
  )
*)
(*e: function Diff_basic.diff *)

(*****************************************************************************)
(* Tests/Bench *)
(*****************************************************************************)
(* good bench: sqlite aggalmation, more than 200 000 LOC 
 * time ./ogit test diff /tmp/test1.c /tmp/test2.c 
 *  (where test[12].c = sqlite.c or shorter version of sqlite.c)
 *  on my Macbook Air (from around 2014).
 * 200 000 LOC:
 * - Myers: 1.4s (in ocaml bytecode mode)
 * - Basic: ???
 * 20 000 LOC: (matrix_distance requires a matrix of 400M elements)
 * - Myers: 0.1s 
 * - Basic: 104s in bytecode, 33s in native
 *)

(*
let edit_distance s1 s2 = 
  (matrix_distance s1 s2).(String.length s1).(String.length s2)


let test = edit_distance "vintner" "writers"
let _ = assert (edit_distance "winter" "winter" = 0)
let _ = assert (edit_distance "vintner" "writers" = 5)
*)

(*e: version_control/diff_basic.ml *)
