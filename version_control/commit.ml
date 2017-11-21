(*s: version_control/commit.ml *)
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
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(*s: type Commit.t *)
type t = {
  tree     : Tree.hash; (* the root *)
  (* first commit has no parent, and merge commits have 2 parents *)
  parents  : hash list;
  (* note that User.t contains a time *)
  author   : User.t;
  message  : string;
  (*s: [[Commit.t]] extra fields *)
  committer: User.t;
  (*e: [[Commit.t]] extra fields *)
  (* less: encoding, gpgsig, mergetag, extra *)
}
(*e: type Commit.t *)
(*s: type Commit.hash *)
and hash = Sha1.t
(*e: type Commit.hash *)

(*****************************************************************************)
(* API *)
(*****************************************************************************)

(* for git log *)

(*s: function Commit.walk_history *)
(* less: sort by time? so have a sorted queue of commits *)
let walk_history read_commit f sha =
  (* we are walking a DAG, so we need to remember already processed nodes *)
  let hdone = Hashtbl.create 101 in
  let rec aux sha =
    if Hashtbl.mem hdone sha
    then ()
    else begin
      Hashtbl.add hdone sha true;
      let commit = read_commit sha in
      (* todo: path matching *)
      f sha commit;
      commit.parents |> List.iter aux
    end
  in
  aux sha
(* 
let walk_graph r f =
  let heads = 
    Repository.all_refs r |> Common.map_filter (fun aref ->
      if aref =~ "refs/heads/"
      then Some (Repository.follow_ref_some r (Refs.Ref aref))
      else None
    )
  in
  ...
  heads |> List.iter aux
*)
(*e: function Commit.walk_history *)

(* for git pull *)

(*s: function Commit.collect_ancestors *)
(* similar to walk_history but with exposed hdone hash *)
let collect_ancestors read_commit top_commits hdone =
  let hcommits = Hashtbl.create 101 in
  let rec aux sha =
    if Hashtbl.mem hdone sha
    then ()
    else begin
      Hashtbl.add hdone sha true;
      Hashtbl.add hcommits sha true;
      let commit = read_commit sha in
      commit.parents |> List.iter aux
    end
  in
  top_commits |> List.iter aux;
  hcommits
(*e: function Commit.collect_ancestors *)


(*****************************************************************************)
(* IO *)
(*****************************************************************************)

(*s: function Commit.read *)
let read ch =
  let tree = 
    IO_.read_key_space_value_newline ch "tree" Hexsha.read in
  (* todo: read "parent" or "author", because first commit has no parent *)
  let parents, author = 
    let rec loop parents =
      let str = IO_.read_string_and_stop_char ch ' ' in
      match str with
      | "parent" -> 
        let v = Hexsha.read ch in
        let c = IO.read ch in
        if c <> '\n'
        then failwith "Commit.read: missing newline after parent";
        loop (v::parents)
      | "author" ->
        let v = User.read ch in
        let c = IO.read ch in
        if c <> '\n'
        then failwith "Commit.read: missing newline after author";
        List.rev parents, v
      | _ -> failwith (spf "Commit.read: was expecting parent or author not %s"
                         str)
    in
    loop []
  in
  let committer = 
    IO_.read_key_space_value_newline ch "committer" User.read in
  let c = IO.read ch in
  if c <> '\n'
  then failwith "Commit.read: missing newline before message";
  let msg = IO.read_all ch in
  { tree = Hexsha.to_sha tree; 
    parents = parents |> List.map Hexsha.to_sha; 
    author = author; committer = committer;
    message = msg;
  }
(*e: function Commit.read *)

(*s: function Commit.write *)
let write commit ch =
  IO.nwrite ch "tree ";
  Hexsha.write ch (Hexsha.of_sha commit.tree);
  IO.write ch '\n';
  commit.parents |> List.iter (fun parent ->
    IO.nwrite ch "parent ";
    Hexsha.write ch (Hexsha.of_sha parent);
    IO.write ch '\n';
  );
  IO.nwrite ch "author ";
  User.write ch commit.author;
  IO.write ch '\n';
  IO.nwrite ch "committer ";
  User.write ch commit.committer;
  IO.write ch '\n';

  IO.write ch '\n';
  IO.nwrite ch commit.message
(*e: function Commit.write *)

(*****************************************************************************)
(* Show *)
(*****************************************************************************)

(*s: function Commit.show *)
let show x =
  pr (spf "Author: %s <%s>" x.author.User.name x.author.User.email);
  (* less: date of author or committer? *)
  let date = x.author.User.date in
  pr (spf "Date:   %s" (User.string_of_date date));
  pr "";
  pr ("    " ^ x.message)
  (* showing diff done in caller in Cmd_show.show *)        
(*e: function Commit.show *)
(*e: version_control/commit.ml *)
