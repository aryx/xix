(* Copyright 2016 Yoann Padioleau, see copyright.txt *)
open Common

module A = Ast

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* A poor's man regexp system. 
 *
 * alt: could reuse Str and just have a regexp_of_word that
 * transform a word pattern containing % in a regular regexp.
 *) 

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

exception TooManyPercents
exception PercentNotFound

let rec string_after_percent xs =
  match xs with
  | [] -> ""
  | x::xs ->
    (match x with 
    | A.String s -> s ^ string_after_percent xs
    | A.Percent -> raise TooManyPercents
    | _ -> raise (Impossible "see eval_partial_word")
    )

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

(* pre: should have called eval_partial_word so the word should contain
 * only String or Percent.
 * ex: match_ [String "foo"; Percent; String ".c"] "foobar.c"
 * This is arguably (and sadly) more complicated than the C code.
 *)
let rec match_ word str =
  let len = String.length str in
  match word with
  | [] -> raise PercentNotFound
  | x::xs ->
    (match x with
    | A.String s ->
        let len2 = String.length s in
        if len2 > len
        then None
        else
          if s <> (String.sub str 0 len2)
          then None
          else match_ xs (String.sub str len2 (len - len2))
    | A.Percent ->
        let str_word_after = string_after_percent xs in
        let len_after = String.length str_word_after in
        let len_matching_percent = len - len_after in
        let stem = String.sub str 0 len_matching_percent in
        if str_word_after = String.sub str len_matching_percent 
          (len - len_matching_percent) && stem <> ""
        then Some stem
        else None
    | _ -> raise (Impossible "see eval_partial_word")
    )        

let subst word stem =
  word |> List.map (function
    | A.String s -> s
    | A.Percent -> stem
    | _ -> raise (Impossible "see eval_partial_word")
  ) |> String.concat ""


let match_and_subst pat sub str =
  match match_ pat str with
  | Some stem -> subst sub stem
  | None -> str
