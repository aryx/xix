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
 * in index.ml and git_unix.ml
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
  (* relative path *)
  name  : Common.filename;
}

(* less: extensions *)

(* the entries are sorted (see compare_entries below) *)
type t = entry list

let empty = []

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

let entry_of_stat stats relpath sha =
  let stat_info = 
    { ctime = { lsb32 = Int32.of_float stats.Unix.st_ctime; nsec = 0l };
      mtime = { lsb32 = Int32.of_float stats.Unix.st_mtime; nsec = 0l };
      dev = Int32.of_int stats.Unix.st_dev;
      inode = Int32.of_int stats.Unix.st_ino;
      mode = 
        (match stats.Unix.st_kind, stats.Unix.st_perm with
        | Unix.S_REG, p -> 
          if p land 0o100 = 0o100 
          then Exec 
          else Normal
        | Unix.S_LNK, _ -> Link
        | _ -> failwith (spf "unsupported file type %s" relpath)
        );
      uid = Int32.of_int stats.Unix.st_uid;
      gid = Int32.of_int stats.Unix.st_gid;
      size = Int32.of_int stats.Unix.st_size;
    }
  in
  { stats = stat_info;
    id = sha;
    stage = 0; (* TODO? *)
    name = relpath
  }

(*****************************************************************************)
(* Add/Del *)
(*****************************************************************************)
let rec remove idx name =
  match idx with
  | [] -> failwith (spf "The file %s is not in the index" name)
  | x::xs ->
    (match compare name x.name with
    | 1 -> x::(remove xs name)
    | 0 -> xs
    (* the entries are sorted *)
    | -1 -> failwith (spf "The file %s is not in the index" name)
    | x -> raise (Impossible (spf "compare can not return %d" x))
    )

let rec add idx entry =
  match idx with
  | [] -> [entry]
  | x::xs ->
    (match compare entry.name x.name with
    | 1 -> x::(add xs entry)
    (* replace old entry is ok *)
    | 0 -> entry::xs
    (* the entries are sorted *)
    | -1 -> entry::x::xs
    | x -> raise (Impossible (spf "compare can not return %d" x))
    )


(*****************************************************************************)
(* IO *)
(*****************************************************************************)

let read_time ch =
  (* less: unsigned actually *)
  let lsb32 = IO.BigEndian.read_real_i32 ch in
  let nsec = IO.BigEndian.read_real_i32 ch in
  { lsb32; nsec }

let write_time ch time =
  IO.BigEndian.write_real_i32 ch time.lsb32;
  IO.BigEndian.write_real_i32 ch time.nsec

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

let write_mode ch mode =
  IO.BigEndian.write_ui16 ch 0;
  let n = 
    match mode with
    | Exec    -> 0b1000__000__111_101_101
    | Normal  -> 0b1000__000__110_100_100
    | Link    -> 0b1010__000__000_000_000
    | Gitlink -> 0b1110__000__000_000_000 
  in
  IO.BigEndian.write_ui16 ch n

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

let write_stat_info ch stats =
  write_time ch stats.ctime;
  write_time ch stats.mtime;
  IO.BigEndian.write_real_i32 ch stats.dev;
  IO.BigEndian.write_real_i32 ch stats.inode;
  write_mode ch stats.mode;
  IO.BigEndian.write_real_i32 ch stats.uid;
  IO.BigEndian.write_real_i32 ch stats.gid;
  IO.BigEndian.write_real_i32 ch stats.size;
  ()
 

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

let write_entry ch e =
  write_stat_info ch e.stats;
  Sha1.write ch e.id;
  let flags = (e.stage lsl 12 + String.length e.name) land 0x3FFF in
  IO.BigEndian.write_ui16 ch flags;
  IO.nwrite ch e.name;
  let len = 63 + String.length e.name in
  let pad = 
    match len mod 8 with
    | 0 -> 0
    | n -> 8-n 
  in
  IO.nwrite ch (Bytes.make pad '\000');
  IO.write ch '\000'




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


let write idx ch =
  let n = List.length idx in
  let body =
    IO.output_bytes () |> IO_utils.with_close_out (fun ch ->
      IO.nwrite ch "DIRC";
      IO.BigEndian.write_i32 ch 2;
      IO.BigEndian.write_i32 ch n;
      idx |> List.iter (write_entry ch)
    )
  in
  let sha = Sha1.sha1 body in
  IO.nwrite ch body;
  Sha1.write ch sha
