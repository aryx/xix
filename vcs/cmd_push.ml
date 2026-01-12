(*s: version_control/cmd_push.ml *)
(*s: copyright ocamlgit *)
(* Copyright 2017 Yoann Padioleau, see copyright.txt *)
(*e: copyright ocamlgit *)
open Common
open Fpath_.Operators

(*s: function [[Cmd_push.push]] *)
(* =~ git fetch + git merge but inverting dst and src  *)
let push (caps : < Cap.stdout; Cap.open_out; Cap.open_in; ..>) src_repo
     (url_dst : string) =
  let url = src_repo.Repository.worktree in
  let dst = Repository.open_ (Fpath.v url_dst) in
  (* todo: detect if clean repo? status is empty? *)
  let client = Clients.client_of_url caps !!url in


  (* less: allow to grab from multiple heads, not just HEAD *)
  let remote_HEAD_sha = client.Client.fetch dst in
  
  (* detect if need merge, if current HEAD not parent of new HEAD *)
  let current_HEAD_sha = Repository.follow_ref_some caps dst (Refs.Head) in
  let ancestors_remote_HEAD = 
    Commit.collect_ancestors (Repository.read_commit dst) [remote_HEAD_sha]
      (Hashtbl.create 101)
  in
  (match () with
  | _ when current_HEAD_sha = remote_HEAD_sha -> ()
  | _ when Hashtbl.mem ancestors_remote_HEAD current_HEAD_sha ->
    (* easy case *)
    Console.print caps (spf "fast forward to %s" (Hexsha.of_sha remote_HEAD_sha));
    Repository.set_ref caps dst (Refs.Head) remote_HEAD_sha;
    let commit = Repository.read_commit dst remote_HEAD_sha in
    let tree = Repository.read_tree dst (commit.Commit.tree) in
    Repository.set_worktree_and_index_to_tree caps dst tree
  | _ -> failwith "TODO: git pull need merge"
  )
(*e: function [[Cmd_push.push]] *)

(*s: constant [[Cmd_push.cmd]] *)
let cmd = { Cmd_.
  name = "push";
  usage = " [options] [<url repository>]";
  options = [
    (* less: --all, --force, --progress *)
  ];
  f = (fun caps args ->
    let src, _ = Repository.find_root_open_and_adjust_paths [] in
    match args with
    | [] -> 
      failwith "TODO: use remote information in config file"
    | [url] -> 
      push caps src url
    | _ -> raise Cmd_.ShowUsage
  );
}
(*e: constant [[Cmd_push.cmd]] *)
(*e: version_control/cmd_push.ml *)
