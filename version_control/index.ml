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

(** The type for file-system stat information. *)
type stat_info = {
  ctime: time;
  mtime: time;
  dev  : Int32.t;
  inode: Int32.t;
  mode : mode;
  uid  : Int32.t;
  gid  : Int32.t;
  size : Int32.t;
}
  and mode =
    | Normal
    | Exec
    | Link
    | Gitlink
  (** The type for a time represented by its [lsb32] and [nsec] parts. *)
  and time = {
    lsb32: Int32.t;
    nsec : Int32.t;
  }
    
(** The type for a Git index entry. *)
type entry = {
  stats : stat_info;
  id    : Blob.hash;
  stage : int;
  name  : string;
}

(* less: extensions *)

(* the entries are sorted (see compare_entries below) *)
type t = entry list

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(*
(* Index entries are sorted by the byte sequence that comprises the
   entry name; with a secondary comparison of the stage bits from the
   <ENTRY_FLAGS> if the entry name byte sequences are identical. *)
let compare_entries e1 e2 =
  match String.compare e1.name e2.name with
  | 0 -> Hash.Blob.compare e2.id e1.id
  | i -> i
*)

(*****************************************************************************)
(* IO *)
(*****************************************************************************)

let read_time ch =
  (* less: unsigned actually *)
  let lsb32 = IO.BigEndian.read_real_i32 ch in
  let nsec = IO.BigEndian.read_real_i32 ch in
  { lsb32; nsec }

let read_mode ch =
  let _zero = IO.BigEndian.read_ui16 ch in
  let n = IO.BigEndian.read_ui16 ch in
  match n lsr 12 with
  | 0b1010 -> Link
  | 0b1110 -> Gitlink
  | 0b1000 ->
    (match n land 0x1FF with
    | 0o755 -> Exec
    | 0o644 -> Normal
    | d     -> failwith (spf "Index.mode: invalid permission (%d)" d)
    )
  | m -> failwith (spf "Index.mode: invalid (%d)" m)

let read_stat_info ch =
  let ctime = read_time ch in
  let mtime = read_time ch in
  (* less: unsigned again *)
  let dev = IO.BigEndian.read_real_i32 ch in
  let inode = IO.BigEndian.read_real_i32 ch in
  let mode = read_mode ch in
  let uid = IO.BigEndian.read_real_i32 ch in
  let gid = IO.BigEndian.read_real_i32 ch in
  let size = IO.BigEndian.read_real_i32 ch in
  { mtime; ctime; dev; inode; mode; uid; gid; size }

let read_entry ch =
  let stats = read_stat_info ch in
  let id = Sha1.read ch in
  let stage, len =
    let i = IO.BigEndian.read_ui16 ch in
    (i land 0x3000) lsr 12,
    (i land 0x0FFF)
  in
  let name = IO.really_nread_string ch len in
  let c = IO.read ch in
  if c <> '\000'
  then failwith "Index.read_entry: expecting null char after name";
  let len = 63 + String.length name in
  let pad = 
    match len mod 8 with
    | 0 -> 0
    | n -> 8-n 
  in
  let _zeros = IO.really_nread ch pad in
  (* less: assert zeros *)
  { stats; id; stage; name }

let read_entries ch =
  let n = IO.BigEndian.read_i32 ch in
  let rec loop acc n =
    if n = 0 
    then List.rev acc
    else
      let entry = read_entry ch in
      loop (entry :: acc) (n - 1) in
  loop [] n

let read ch =
  let header = IO.really_nread_string ch 4 in
  if header <> "DIRC"
  then failwith "Index.read: expecting DIRC header";
  let version = IO.BigEndian.read_i32 ch in
  if version <> 2
  then failwith "Index.read: expecting version 2";
  let entries = read_entries ch in
  (* todo: read_extensions but need know when reach last 20 bytes *)
  (* todo: check hash correctly stored in last 20 bytes *)
  entries
