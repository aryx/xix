(* Copyright 2017 Yoann Padioleau, see copyright.txt *)
open Common

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type t = 
  | Blob of Blob.t
  | Commit of Commit.t
  | Tree of Tree.t
(*  | Tag of Tag.t *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(*****************************************************************************)
(* IO *)
(*****************************************************************************)

let read ch =
  let str = IO_utils.read_string_and_stop_char ch ' ' in
  let n = IO_utils.read_int_and_nullbyte ch in
  let raw = IO.really_nread ch n in
  (* less: assert finished ch? use IO.pos_in? *)
  let ch2 = IO.input_bytes raw in
  (* less: just reuse ch so avoid use of intermediate strings? *)
  match str with
  | "blob"   -> Blob   (Blob.read ch2)
  | "commit" -> Commit (Commit.read ch2)
  | "tree"   -> Tree   (Tree.read ch2)
  (* "tag" -> Tag (Tag.read raw) *)
  (* less: assert finished ch2? *)
  | str -> failwith (spf "Objects.read: invalid header: %s" str)

