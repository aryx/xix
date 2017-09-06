(*s: version_control/cmd_checkout.ml *)
(* Copyright 2017 Yoann Padioleau, see copyright.txt *)
open Common

(*s: function Cmd_checkout.checkout *)
let checkout r str =
  let all_refs = Repository.all_refs r in
  let refname = "refs/heads/" ^ str in
  match () with
  | _ when List.mem refname all_refs ->
    let commitid = Repository.follow_ref_some r (Refs.Ref refname) in
    let commit = Repository.read_commit r commitid in
    let treeid = commit.Commit.tree in
    let tree = Repository.read_tree r treeid in
    (* todo: order of operation? set ref before index? reverse? *)
    Repository.write_ref r (Refs.Head) (Refs.OtherRef refname);
    Repository.set_worktree_and_index_to_tree r tree;
    pr (spf "Switched to branch '%s'" str);
    (* less: if master, then check if up-to-date with origin/master *)
  | _ when Hexsha.is_hexsha str ->
    let commitid = Hexsha.to_sha str in
    let commit = Repository.read_commit r commitid in
    let treeid = commit.Commit.tree in
    let tree = Repository.read_tree r treeid in
    (* todo: order of operation? set ref before index? reverse? *)
    Repository.write_ref r (Refs.Head) (Refs.Hash commitid);
    Repository.set_worktree_and_index_to_tree r tree;
    pr (spf "Note: checking out '%s'." str);
    pr ("You are in 'detached HEAD' state");
  | _ -> raise Cmd.ShowUsage
(*e: function Cmd_checkout.checkout *)


(*s: function Cmd_checkout.update *)
(* Your branch is up-to-date with 'origin/master'. *)
let update r =
  raise Todo
(*e: function Cmd_checkout.update *)

(*s: constant Cmd_checkout.cmd *)
let cmd = { Cmd.
  name = "checkout";
  help = " [options] <branch>
   or: ogit checkout [options] <commitid>
   or: ogit checkout [options]
";
  options = [
    (* less: --detach, --patch?
     * -b create and checkout a branch
     *)
  ];
  f = (fun args ->
    (* todo: allow git rm from different location *)
    let r = Repository.open_ "." in
    match args with
    | [] -> update r
    | [str] -> checkout r str
    | _ -> raise Cmd.ShowUsage
  );
}
(*e: constant Cmd_checkout.cmd *)
(*e: version_control/cmd_checkout.ml *)
