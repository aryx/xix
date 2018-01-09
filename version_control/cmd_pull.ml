(*s: version_control/cmd_pull.ml *)
(*s: copyright ocamlgit *)
(* Copyright 2017 Yoann Padioleau, see copyright.txt *)
(*e: copyright ocamlgit *)
open Common

(*s: function [[Cmd_pull.pull]] *)
(* =~ git fetch + git merge *)
let pull dst url =
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
  | _ when remote_HEAD_sha = current_HEAD_sha -> ()
  | _ when Hashtbl.mem ancestors_remote_HEAD current_HEAD_sha ->
    (* easy case *)
    pr (spf "fast forward to %s" (Hexsha.of_sha remote_HEAD_sha));
    Repository.set_ref dst (Refs.Head) remote_HEAD_sha;
    let commit = Repository.read_commit dst remote_HEAD_sha in
    let tree = Repository.read_tree dst (commit.Commit.tree) in
    Repository.set_worktree_and_index_to_tree dst tree
  | _ -> failwith "TODO: git pull need merge"
  )
(*e: function [[Cmd_pull.pull]] *)

(*s: constant [[Cmd_pull.cmd]] *)
let cmd = { Cmd.
  name = "pull";
  usage = " [options] [<url repository>]";
  options = [
  ];
  f = (fun args ->
    let dst, _ = Repository.find_root_open_and_adjust_paths [] in
    match args with
    | [] -> 
      failwith "TODO: use remote information in config file"
    | [url] -> 
      pull dst url
    | _ -> raise Cmd.ShowUsage
  );
}
(*e: constant [[Cmd_pull.cmd]] *)
(*e: version_control/cmd_pull.ml *)
