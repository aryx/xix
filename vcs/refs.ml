(*s: version_control/refs.ml *)
open Common
open Regexp.Operators

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(*s: type [[Refs.refname]] *)
(* should always start with "refs/", see is_refname() later *)
type refname = string (* e.g. "refs/heads/master" *)
(*e: type [[Refs.refname]] *)

(*s: type [[Refs.t]] *)
type t =
  | Head
  | Ref of refname
(*e: type [[Refs.t]] *)

(*s: type [[Refs.ref_content]] *)
type ref_content =
  (* the final value when follow all the pointers *)
  | Hash of Commit.hash
  (* pointer (may contain sha1 or another pointer again) *)
  | OtherRef of refname
(*e: type [[Refs.ref_content]] *)

(*s: constant [[Refs.default_head_content]] *)
let default_head_content = 
  OtherRef "refs/heads/master"
(*e: constant [[Refs.default_head_content]] *)

(*s: function [[Refs.is_valid_refname]] *)
let is_refname str =
  str =~ "^refs/"
  (* todo: git-check-ref-format *)
(*e: function [[Refs.is_valid_refname]] *)

(*****************************************************************************)
(* Dumper *)
(*****************************************************************************)

(*s: function [[Refs.string_of_ref]] *)
let string_of_ref = function
  | Head -> "HEAD"
  | Ref x -> x
(*e: function [[Refs.string_of_ref]] *)

(*****************************************************************************)
(* IO *)
(*****************************************************************************)

(*s: function [[Refs.read]] *)
let read ch =
  let str = IO.read_all ch in
  (* less: check finish by newline? *)
  match str with
  | _ when str =~ "^ref: \\(.*\\)$" -> OtherRef (Regexp.matched1 str)
  | _ -> Hash (str |> IO.input_string |> Hexsha.read |> Hexsha.to_sha)
(*e: function [[Refs.read]] *)

(*s: function [[Refs.write]] *)
let write content ch =
  match content with
  | Hash h -> 
    IO.nwrite_string ch (Hexsha.of_sha h ^ "\n")
  | OtherRef name ->
    IO.nwrite_string ch ("ref: " ^ name ^ "\n")
(*e: function [[Refs.write]] *)
(*e: version_control/refs.ml *)
