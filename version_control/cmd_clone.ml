(*s: version_control/cmd_clone.ml *)
(*s: copyright ocamlgit *)
(* Copyright 2017 Yoann Padioleau, see copyright.txt *)
(*e: copyright ocamlgit *)
open Common

(*s: function Cmd_clone.clone *)
(* =~ git pull from scratch (itself =~ git fetch + git merge) *)
let clone url path_dst =
  let client = Clients.client_of_url url in
  
  Repository.init path_dst;
  let dst = Repository.open_ path_dst in

  (* less: allow to grab from different head? *)
  let remote_HEAD_sha = client.Client.fetch dst in
  Repository.set_ref dst (Refs.Head) remote_HEAD_sha;

  (* =~ reset index *)
  let commit = Repository.read_commit dst remote_HEAD_sha in
  let tree = Repository.read_tree dst (commit.Commit.tree) in
  Repository.set_worktree_and_index_to_tree dst tree
(*e: function Cmd_clone.clone *)

(* todo: when clone then repo should have a "refs/remotes/origin/master" *)

(*s: constant Cmd_clone.cmd *)
let cmd = { Cmd.
  name = "clone";
  help = " [options] <repo> [<dir>]";
  options = [
    (* less: --bare, --progress, --depth *)
  ];
  f = (fun args ->
    match args with
    | [url] -> 
      clone url "."
    | [url;dst] ->
      clone url dst
    | _ -> raise Cmd.ShowUsage
  );
}
(*e: constant Cmd_clone.cmd *)
(*e: version_control/cmd_clone.ml *)
