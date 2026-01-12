(*s: version_control/cmd_clone.ml *)
(*s: copyright ocamlgit *)
(* Copyright 2017 Yoann Padioleau, see copyright.txt *)
(*e: copyright ocamlgit *)

(*s: function [[Cmd_clone.clone]] *)
(* =~ git pull from scratch (itself =~ git fetch + git merge) *)
let clone caps url (path_dst : Fpath.t) =
  let client = Clients.client_of_url caps url in
  
  Repository.init caps path_dst;
  let dst = Repository.open_ caps path_dst in

  (* less: allow to grab from different head? *)
  let remote_HEAD_sha = client.Client.fetch dst in
  Repository.set_ref caps dst (Refs.Head) remote_HEAD_sha;

  (* =~ reset index *)
  let commit = Repository.read_commit dst remote_HEAD_sha in
  let tree = Repository.read_tree dst (commit.Commit.tree) in
  Repository.set_worktree_and_index_to_tree caps dst tree
(*e: function [[Cmd_clone.clone]] *)

(* todo: when clone then repo should have a "refs/remotes/origin/master" *)

(*s: constant [[Cmd_clone.cmd]] *)
let cmd = { Cmd_.
  name = "clone";
  usage = " [options] <repo> [<dir>]";
  options = [
    (* less: --bare, --progress, --depth *)
  ];
  f = (fun caps args ->
    match args with
    | [url]     -> clone caps url (Fpath.v ".")
    | [url;dst] -> clone caps url (Fpath.v dst)
    | _ -> raise Cmd_.ShowUsage
  );
}
(*e: constant [[Cmd_clone.cmd]] *)
(*e: version_control/cmd_clone.ml *)
