(*s: version_control/cmd_push.ml *)
(* Copyright 2017 Yoann Padioleau, see copyright.txt *)
open Common

(*s: function Cmd_push.push *)
(* =~ git fetch + git merge but inverting dst and src  *)
let push src_repo url_dst =
  let url = src_repo.Repository.worktree in
  let dst = Repository.open_ url_dst in
  (* todo: detect if clean repo? status is empty? *)
  let client = Clients.client_of_url url in


  (* less: allow to grab from multiple heads, not just HEAD *)
  let remote_HEAD_sha = client.Client.fetch dst in
  
  (* detect if need merge, if current HEAD not parent of new HEAD *)
  let current_HEAD_sha = Repository.follow_ref_some dst (Refs.Head) in
  let ancestors_remote_HEAD = 
    Commit.collect_ancestors (Repository.read_commit dst) [remote_HEAD_sha]
      (Hashtbl.create 101)
  in
  (match () with
  | _ when current_HEAD_sha = remote_HEAD_sha -> ()
  | _ when Hashtbl.mem ancestors_remote_HEAD current_HEAD_sha ->
    (* easy case *)
    pr (spf "fast forward to %s" (Hexsha.of_sha remote_HEAD_sha));
    Repository.set_ref dst (Refs.Head) remote_HEAD_sha;
    let commit = Repository.read_commit dst remote_HEAD_sha in
    let tree = Repository.read_tree dst (commit.Commit.tree) in
    Repository.set_worktree_and_index_to_tree dst tree
  | _ -> failwith "TODO: git pull need merge"
  )
(*e: function Cmd_push.push *)

(*s: constant Cmd_push.cmd *)
let cmd = { Cmd.
  name = "push";
  help = " [options] [<url repository>]";
  options = [
    (* less: --all, --force, --progress *)
  ];
  f = (fun args ->
    (* todo: allow git rm from different location *)
    let src = Repository.open_ "." in
    match args with
    | [] -> 
      failwith "TODO: use remote information in config file"
    | [url] -> 
      push src url
    | _ -> raise Cmd.ShowUsage
  );
}
(*e: constant Cmd_push.cmd *)
(*e: version_control/cmd_push.ml *)
