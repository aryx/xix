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
  let str = IO.read_all ch in
  (* less: check finish by newline? *)
  match str with
  | _ when str =~ "^ref: \\(.*\\)$" -> OtherRef (Regexp_.matched1 str)
  | _ -> Hash (str |> IO.input_string |> Hexsha.read |> Hexsha.to_sha)

let write content ch =
  match content with
  | Hash h -> 
    IO.nwrite_string ch (Hexsha.of_sha h ^ "\n")
  | OtherRef name ->
    IO.nwrite_string ch ("ref: " ^ name ^ "\n")
