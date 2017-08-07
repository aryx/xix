(* Copyright 2017 Yoann Padioleau, see copyright.txt *)
open Common

(* old: was called get_transport_and_path (and xxx_from_url in dulwich *)
(* less: move in clients.ml? a bit like cmd.ml and cmds.ml *)
let client_of_url url =
  match url with
  (* less: should use URL parsing library *)
  | s when s =~ "^git://" -> 
    Client_git.mk_client url
  | s when s =~ "^ssh://" -> 
    failwith "ssh not supported"
  | s when s =~ "^http://" -> 
    failwith "http not supported"
  | s -> 
    if Sys.file_exists s
    then Client_local.mk_client url
    else failwith (spf "remote repository url not supported: %s" url)

(* =~ git fetch + git merge *)
let pull dst url =
  let client = client_of_url url in
  (* less: allow to grab from multiple heads, not just HEAD *)
  let remote_HEAD_sha = client.Client.fetch dst in

  (* todo: detect if need merge, if current HEAD not parent of new HEAD *)
  Repository.set_ref dst (Refs.Head) remote_HEAD_sha;

  let commit = Repository.read_commit dst remote_HEAD_sha in
  let tree = Repository.read_tree dst (commit.Commit.tree) in
  (* todo: detect if need merge, or fast-forward *)
  Repository.set_worktree_and_index_to_tree dst tree


let cmd = { Cmd.
  name = "pull";
  help = " [options] [<url repository>]";
  options = [
  ];
  f = (fun args ->
    (* todo: allow git rm from different location *)
    let dst = Repository.open_ "." in
    match args with
    | [] -> 
      failwith "TODO: use remote information in config file"
    | [url] -> 
      pull dst url
    | _ -> raise Cmd.ShowUsage
  );
}
