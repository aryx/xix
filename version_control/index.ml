(*s: version_control/index.ml *)
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
open Common

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* 
 * Most of the code below derives from: https://github.com/mirage/ocaml-git
 * in index.ml and git_unix.ml
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(*s: type Index.stat_info *)
(** The type for file-system stat information. *)
type stat_info = {
  mode : mode;

  ctime: time;
  mtime: time;

  dev  : Int32.t;
  inode: Int32.t;

  uid  : Int32.t;
  gid  : Int32.t;

  size : Int32.t;
}
(*e: type Index.stat_info *)
(*s: type Index.mode *)
  and mode =
    (* no directory here *)
    | Normal
    | Exec
    | Link

    | Gitlink (*?? submodule? *)
(*e: type Index.mode *)
(*s: type Index.time *)
  (** The type for a time represented by its [lsb32] and [nsec] parts. *)
  and time = {
    lsb32: Int32.t;
    nsec : Int32.t;
  }
(*e: type Index.time *)
    
(*s: type Index.entry *)
(** The type for a Git index entry. *)
type entry = {
  (* relative path *)
  name  : Common.filename;
  id    : Blob.hash;
  stats : stat_info;
  stage : int; (*?? *)
}
(*e: type Index.entry *)

(* less: extensions *)

(*s: type Index.t *)
(* the entries are sorted (see compare_entries below) *)
type t = entry list
(*e: type Index.t *)

(*s: constant Index.empty *)
let empty = []
(*e: constant Index.empty *)

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

(*s: function Index.stat_info_of_lstats *)
let stat_info_of_lstats stats = 
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
        | _ -> failwith (spf "unsupported file type")
        );
      uid = Int32.of_int stats.Unix.st_uid;
      gid = Int32.of_int stats.Unix.st_gid;
      size = Int32.of_int stats.Unix.st_size;
    }
(*e: function Index.stat_info_of_lstats *)

(*s: function Index.mk_entry *)
let mk_entry relpath sha stats =
  { name = relpath;
    id = sha;
    stats = stat_info_of_lstats stats;
    stage = 0; (* TODO? *)
  }
(*e: function Index.mk_entry *)

(*s: function Index.perm_of_mode *)
let perm_of_mode mode = 
  match mode with
  | Normal -> Tree.Normal
  | Exec -> Tree.Exec
  | Link -> Tree.Link
  | Gitlink -> Tree.Commit (* sure? *)
(*e: function Index.perm_of_mode *)

(*s: function Index.mode_of_perm *)
let mode_of_perm perm = 
  match perm with
  | Tree.Normal -> Normal
  | Tree.Exec -> Exec
  | Tree.Link -> Link
  | Tree.Commit -> Gitlink
  | Tree.Dir -> failwith "index entry does not support Tree.dir perm"
(*e: function Index.mode_of_perm *)

(*****************************************************************************)
(* Add/Del *)
(*****************************************************************************)

(*s: function Index.remove_entry *)
let rec remove_entry idx name =
  match idx with
  | [] -> failwith (spf "The file %s is not in the index" name)
  | x::xs ->
    (match name <=> x.name with
    | Sup -> x::(remove_entry xs name)
    | Equal -> xs
    (* the entries are sorted *)
    | Inf -> failwith (spf "The file %s is not in the index" name)
    )
(*e: function Index.remove_entry *)

(*s: function Index.add_entry *)
let rec add_entry idx entry =
  match idx with
  | [] -> [entry]
  | x::xs ->
    (match entry.name <=> x.name with
    | Sup -> x::(add_entry xs entry)
    (* replacing old entry is ok *)
    | Equal -> entry::xs
    (* the entries are sorted *)
    | Inf -> entry::x::xs
    )
(*e: function Index.add_entry *)

(*****************************************************************************)
(* tree of index *)
(*****************************************************************************)

(*s: type Index.dir *)
type dir = dir_entry list ref
(*e: type Index.dir *)
(*s: type Index.dir_entry *)
  and dir_entry =
    | Subdir of string (* base *)
    | File of string (* base *) * entry
(*e: type Index.dir_entry *)
(*s: type Index.dirs *)
type dirs = (string (* full relpath of dir *), dir) Hashtbl.t
(*e: type Index.dirs *)

(* the code in this section derives from dulwich *)

(*s: function Index.add_dir *)
let rec add_dir dirs dirpath =
  try 
    Hashtbl.find dirs dirpath
  with Not_found ->
    let newdir = ref [] in
    Hashtbl.add dirs dirpath newdir;
    let (parent, base) = 
      Filename.dirname dirpath, Filename.basename dirpath in
    (* !recursive call! should stop at some point because "." is in dirs *)
    let dir = add_dir dirs parent in
    dir := Subdir (base)::!dir;
    newdir
(*e: function Index.add_dir *)

(*s: function Index.build_trees *)
let rec build_trees dirs dirpath add_tree_obj =
  let dir = Hashtbl.find dirs dirpath in
  (* entries of a Tree.t must be sorted, but entries of an index too,
   * so we can assume add_dir was called in sorted order
   *)
  let xs = List.rev !dir in
  let tree = 
    xs |> List.map (function
      | File (base, entry) ->
        {Tree.
         name = base; 
         node = entry.id; 
         perm = perm_of_mode entry.stats.mode;
        }
      | Subdir base ->
        let sha = 
          build_trees dirs (Filename.concat dirpath base) add_tree_obj in
        {Tree. perm = Tree.Dir; name = base; node = sha }
    )
  in
  add_tree_obj tree
(*e: function Index.build_trees *)


(*s: function Index.tree_of_index *)
let tree_of_index idx add_tree_obj =
  let (dirs: dirs) = Hashtbl.create 11 in
  Hashtbl.add dirs "." (ref []);
  (* populate dirs *)
  idx |> List.iter (fun entry ->
    let relpath = entry.name in
    let (dir, base) = Filename.dirname relpath, Filename.basename relpath in
    let dir = add_dir dirs dir in
    dir := (File (base, entry))::!dir
  );
  (* build trees *)
  build_trees dirs "." add_tree_obj
(*e: function Index.tree_of_index *)

(*****************************************************************************)
(* index of tree *)
(*****************************************************************************)
(* See repository.set_worktree_and_index_to_tree() *)

(*****************************************************************************)
(* IO *)
(*****************************************************************************)

(*s: function Index.read_time *)
let read_time ch =
  (* less: unsigned actually *)
  let lsb32 = IO.BigEndian.read_real_i32 ch in
  let nsec = IO.BigEndian.read_real_i32 ch in
  { lsb32; nsec }
(*e: function Index.read_time *)

(*s: function Index.write_time *)
let write_time ch time =
  IO.BigEndian.write_real_i32 ch time.lsb32;
  IO.BigEndian.write_real_i32 ch time.nsec
(*e: function Index.write_time *)

(*s: function Index.read_mode *)
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
(*e: function Index.read_mode *)

(*s: function Index.write_mode *)
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
(*e: function Index.write_mode *)

(*s: function Index.read_stat_info *)
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
(*e: function Index.read_stat_info *)

(*s: function Index.write_stat_info *)
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
(*e: function Index.write_stat_info *)
 

(*s: function Index.read_entry *)
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
(*e: function Index.read_entry *)

(*s: function Index.write_entry *)
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
(*e: function Index.write_entry *)




(*s: function Index.read_entries *)
let read_entries ch =
  let n = IO.BigEndian.read_i32 ch in
  let rec loop acc n =
    if n = 0 
    then List.rev acc
    else
      let entry = read_entry ch in
      loop (entry :: acc) (n - 1) in
  loop [] n
(*e: function Index.read_entries *)

(*s: function Index.read *)
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
(*e: function Index.read *)


(*s: function Index.write *)
let write idx ch =
  let n = List.length idx in
  let body =
    IO.output_bytes () |> IO_.with_close_out (fun ch ->
      IO.nwrite ch "DIRC";
      IO.BigEndian.write_i32 ch 2;
      IO.BigEndian.write_i32 ch n;
      idx |> List.iter (write_entry ch)
    )
  in
  let sha = Sha1.sha1 body in
  IO.nwrite ch body;
  Sha1.write ch sha
(*e: function Index.write *)
(*e: version_control/index.ml *)
