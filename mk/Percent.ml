(*s: Percent.ml *)
(* Copyright 2016 Yoann Padioleau, see copyright.txt *)
open Stdcompat (* for |> *)
open Common

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* A poor's man regexp system. 
 *
 * alt: could reuse Str and just have a regexp_of_word that
 * transform a word pattern containing % in a regular regexp.
 *) 

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(*s: type [[Percent.pattern_elem]] *)
type pattern_elem =
  | PStr of string
  | PPercent
(*e: type [[Percent.pattern_elem]] *)
(*s: type [[Percent.pattern]] *)
type pattern = P of pattern_elem list
(*e: type [[Percent.pattern]] *)



(*s: function [[Percent.check_pattern]] *)
let check_pattern (P xs) =
  if xs = []
  then raise (Impossible (spf "empty pattern"));
  xs |> List.iter (function
    | PPercent -> ()
    | PStr "" -> raise (Impossible (spf "empty string element in pattern"));
    | PStr _ -> ()
(*e: function [[Percent.check_pattern]] *)
  )

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(*s: exception [[Percent.TooManyPercents]] *)
exception TooManyPercents
(*e: exception [[Percent.TooManyPercents]] *)
(*s: exception [[Percent.PercentNotFound]] *)
exception PercentNotFound
(*e: exception [[Percent.PercentNotFound]] *)

(*s: function [[Percent.string_after_percent]] *)
let rec string_after_percent xs =
  match xs with
  | [] -> ""
  | x::xs ->
    (match x with 
    | PStr s -> s ^ string_after_percent xs
    | PPercent -> raise TooManyPercents
    )
(*e: function [[Percent.string_after_percent]] *)

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

(*s: function [[Percent.match_]] *)
(* ex: match_ [PStr "foo"; PPercent; PStr ".c"] "foobar.c"
 * This is arguably (and sadly) more complicated than the C code.
 *)

let rec match_ (P pat) str =
  let len = String.length str in
  match pat with
  | [] -> raise PercentNotFound
  | x::xs ->
    (match x with
    | PStr s ->
        let len2 = String.length s in
        if len2 > len
        then None
        else
          if s <> (String.sub str 0 len2)
          then None
          else match_ (P xs) (String.sub str len2 (len - len2))
    | PPercent ->
        let str_pat_after = string_after_percent xs in
        let len_after = String.length str_pat_after in
        let len_matching_percent = len - len_after in
        if len_matching_percent < 0
        then None
        else
          let stem = String.sub str 0 len_matching_percent in
          if str_pat_after = String.sub str len_matching_percent 
            (len - len_matching_percent) && stem <> ""
          then Some stem
          else None
    )        
(*e: function [[Percent.match_]] *)

(*s: function [[Percent.subst]] *)
let subst (P pat) stem =
  pat |> List.map (function
    | PStr s -> s
    | PPercent -> stem
  ) |> String.concat ""
(*e: function [[Percent.subst]] *)


(*s: function [[Percent.match_and_subst]] *)
let match_and_subst pat sub str =
  match match_ pat str with
  | Some stem -> subst sub stem
  | None -> str
(*e: function [[Percent.match_and_subst]] *)
(*e: Percent.ml *)
