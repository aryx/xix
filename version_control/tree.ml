(*s: version_control/tree.ml *)
(*s: copyright ocaml-git *)
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
(*e: copyright ocaml-git *)
open Stdcompat (* for bytes *)
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

(*s: type [[Tree.perm]] *)
(* similar to Index.mode, but with also a 'Dir' *)
type perm = 
  | Normal
  | Dir
  | Exec
  (*s: [[Tree.perm]] cases *)
  | Link
  (*x: [[Tree.perm]] cases *)
  | Commit (* ?? submodule? *)
  (*e: [[Tree.perm]] cases *)
(*e: type [[Tree.perm]] *)

(*s: type [[Tree.entry]] *)
type entry = {
  (* relative to tree, so does not contain any '/', or '.' or '..' *)
  name: string;
  (* Blob.hash or Tree.hash *)
  id: Sha1.t;

  perm: perm;
}
(*e: type [[Tree.entry]] *)

(*s: type [[Tree.t]] *)
(* todo: entries must be sorted! and each name must be unique *)
type t = entry list
(*e: type [[Tree.t]] *)

(*s: type [[Tree.hash]] *)
type hash = Sha1.t
(*e: type [[Tree.hash]] *)

(*****************************************************************************)
(* Walk *)
(*****************************************************************************)
(*s: function [[Tree.walk_tree]] *)
(* we must visit in sorted order, so the caller of walk_tree can rely on 'f'
 * being called in order (so it can easily create for example sorted 
 * index entries while visiting a tree)
 *)
let rec walk_tree read_tree dirpath f xs =
  xs |> List.iter (fun entry ->
    let relpath = Filename.concat dirpath entry.name in
    f relpath entry;
    match entry.perm with
    | Dir ->
      walk_tree read_tree relpath f (read_tree entry.id)
    | Normal | Exec | Link -> ()
    (*s: [[Tree.walk_tree()]] match perm cases *)
    | Commit ->
      failwith "submodule not supported yet"
    (*e: [[Tree.walk_tree()]] match perm cases *)
  )
(*e: function [[Tree.walk_tree]] *)

(*s: function [[Tree.walk_trees]] *)
let rec walk_trees read_tree dirpath f xs ys =
  let g dirpath entry1_opt entry2_opt =
    f dirpath entry1_opt entry2_opt;
    (match entry1_opt, entry2_opt with
    | Some { perm = Dir; name = str; id = sha }, None ->
      walk_trees read_tree (Filename.concat dirpath str) f
        (read_tree sha) []
    | None, Some { perm = Dir; name = str; id = sha } ->
      walk_trees read_tree (Filename.concat dirpath str) f
        [] (read_tree sha)
    | Some { perm = Dir; name = str1; id = sha1 },
      Some { perm = Dir; name = str2; id = sha2 } ->
      assert (str1 = str2);
        (* todo: could skip if sha1 = sha2 here, useful opti *)
        walk_trees read_tree (Filename.concat dirpath str1) f
          (read_tree sha1) (read_tree sha2)
    | None, None -> raise (Impossible "two None in walk_trees.g")
    (* no directories, no need to recurse *)
    | Some _, None
    | None, Some _
    | Some _, Some _
      -> ()
    )
  in
  match xs, ys with
  | [], [] -> ()
  | x::xs, [] ->
    g dirpath (Some x) None;
    walk_trees read_tree dirpath f xs ys
  | [], y::ys ->
    g dirpath None (Some y);
    walk_trees read_tree dirpath f xs ys
  | x::xs, y::ys ->
    (match x.name <=> y.name with
    | Equal -> 
      g dirpath (Some x) (Some y);
      walk_trees read_tree dirpath f xs ys
    | Inf -> 
      g dirpath (Some x) None;
      walk_trees read_tree dirpath f xs (y::ys)
    | Sup ->
      g dirpath None (Some y);
      walk_trees read_tree dirpath f (x::xs) ys
    )
(*e: function [[Tree.walk_trees]] *)

(*****************************************************************************)
(* IO *)
(*****************************************************************************)

(*s: function [[Tree.perm_of_string]] *)
let perm_of_string = function
  | "44"
  | "100644" -> Normal
  | "100755" -> Exec
  | "120000" -> Link
  | "40000"  -> Dir
  (*s: [[Tree.perm_of_string()]] match str cases *)
  | "160000" -> Commit
  (*e: [[Tree.perm_of_string()]] match str cases *)
  | x        -> failwith (spf "Tree.perm_of_string: %s is not a valid perm." x)
(*e: function [[Tree.perm_of_string]] *)

(*s: function [[Tree.string_of_perm]] *)
let string_of_perm = function
  | Normal -> "100644"
  | Exec   -> "100755"
  | Link   -> "120000"
  | Dir    -> "40000"
  (*s: [[Tree.string_of_perm()]] match perm cases *)
  | Commit -> "160000"
  (*e: [[Tree.string_of_perm()]] match perm cases *)
(*e: function [[Tree.string_of_perm]] *)

(*s: function [[Tree.read_entry]] *)
(* todo: should transform some No_more_input exn in something bad,
 * on first one it's ok, but after it means incomplete entry.
 *)
let read_entry ch =
  let perm = IO_.read_string_and_stop_char ch ' ' in
  (* todo: handle escape char in filenames? encode/decode *)
  let name = IO_.read_string_and_stop_char ch '\000' in
  let hash = Sha1.read ch in
  { perm = perm_of_string perm; name = name; id = hash }
(*e: function [[Tree.read_entry]] *)

(*s: function [[Tree.write_entry]] *)
let write_entry ch e =
  IO.nwrite_string ch (string_of_perm e.perm);
  IO.write ch ' ';
  (* todo: handle escape char in filenames? encode/decode *)
  IO.nwrite_string ch e.name;
  IO.write ch '\000';
  Sha1.write ch e.id
(*e: function [[Tree.write_entry]] *)
  

(*s: function [[Tree.read]] *)
let read ch =
  let rec aux acc =
    try 
      (* todo: how diffentiate no more input from wrong input ?
       * pass size ch and use IO.pos_in ?
       *)
      let e = read_entry ch in
      aux (e::acc)
    with IO.No_more_input ->
      List.rev acc
  in
  aux []
(*e: function [[Tree.read]] *)


(*s: function [[Tree.write]] *)
let write t ch =
  t |> List.iter (write_entry ch)
(*e: function [[Tree.write]] *)

(*****************************************************************************)
(* Show *)
(*****************************************************************************)

(*s: function [[Tree.show]] *)
let show xs =
  xs |> List.iter (fun entry ->
    pr (spf "%s%s" entry.name
          (match entry.perm with
          | Dir -> "/"
          | _ -> ""
          ))
  )
(*e: function [[Tree.show]] *)
(*e: version_control/tree.ml *)
