(*
 * Copyright (c) 2013-2017 Thomas Gazagnaire <thomas@gazagnaire.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)
open Common

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* 
 * 
 * Most of the code below derives from: https://github.com/mirage/ocaml-git
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type hash = Sha1.t

type t = {
  tree     : Tree.hash;
  parents  : hash list;
  author   : User.t;
  committer: User.t;

  message  : string;
}

(*****************************************************************************)
(* IO *)
(*****************************************************************************)

let read ch =
  let tree = 
    IO_utils.read_key_space_value_newline ch "tree" Hexsha.read in
  let parent = 
    IO_utils.read_key_space_value_newline ch "parent" Hexsha.read in
  (* todo: read "parent" or "author" *)
  let other_parents = [] in
  let author   = 
    IO_utils.read_key_space_value_newline ch "author" User.read in

  let committer = 
    IO_utils.read_key_space_value_newline ch "committer" User.read in
  let c = IO.read ch in
  if c <> '\n'
  then failwith "Commit.read: missing newline before message";
  let msg = IO.read_all ch in
  { tree = Hexsha.to_sha tree; 
    parents = (parent::other_parents) |> List.map Hexsha.to_sha; 
    author = author; committer = committer;
    message = msg;
  }
