open Common

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* should always start with "refs/", see is_valid_refname later *)
type refname = string (* e.g. "refs/heads/master" *)

type t =
  | Head
  | Ref of refname

type ref_content =
  (* the final value when follow all the pointers *)
  | Hash of Commit.hash
  (* pointer (may contain sha1 or another pointer again) *)
  | OtherRef of refname

let default_head_content = 
  OtherRef "refs/heads/master"

let is_valid_refname str =
  str =~ "^refs/"
  (* todo: git-check-ref-format *)

(*****************************************************************************)
(* IO *)
(*****************************************************************************)

let write content ch =
  match content with
  | Hash h -> 
    IO.nwrite_string ch (Hexsha.of_sha h ^ "\n")
  | OtherRef name ->
    IO.nwrite_string ch ("ref: " ^ name ^ "\n")
