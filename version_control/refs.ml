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
(* Dumper *)
(*****************************************************************************)
let string_of_ref = function
  | Head -> "HEAD"
  | Ref x -> x

(*****************************************************************************)
(* IO *)
(*****************************************************************************)

let read ch =
  (* less: should check end of buffer, or just call IO.read_all! *)
  raise Todo

let write content ch =
  match content with
  | Hash h -> 
    IO.nwrite_string ch (Hexsha.of_sha h ^ "\n")
  | OtherRef name ->
    IO.nwrite_string ch ("ref: " ^ name ^ "\n")
