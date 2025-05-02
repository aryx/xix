(*s: version_control/cmd_checkout.ml *)
(*s: copyright ocamlgit *)
(* Copyright 2017 Yoann Padioleau, see copyright.txt *)
(*e: copyright ocamlgit *)
open Common

(*s: function [[Cmd_checkout.checkout]] *)
let checkout r str =
  let all_refs = Repository.all_refs r in
  let refname = "refs/heads/" ^ str in

  match () with
  | _ when List.mem refname all_refs ->
    let commitid = Repository.follow_ref_some r (Refs.Ref refname) in
    let commit = Repository.read_commit r commitid in
    let tree = Repository.read_tree r commit.Commit.tree in

    (* todo: order of operation? set ref before index? reverse? *)
    Repository.write_ref r (Refs.Head) (Refs.OtherRef refname);
    Repository.set_worktree_and_index_to_tree r tree;
    UConsole.print (spf "Switched to branch '%s'" str);
    (* less: if master, then check if up-to-date with origin/master *)

  (*s: [[Cmd_checkout.checkout()]] cases *)
  | _ when Hexsha.is_hexsha str ->
    let commitid = Hexsha.to_sha str in
    let commit = Repository.read_commit r commitid in
    let treeid = commit.Commit.tree in
    let tree = Repository.read_tree r treeid in
    (* todo: order of operation? set ref before index? reverse? *)
    Repository.write_ref r (Refs.Head) (Refs.Hash commitid);
    Repository.set_worktree_and_index_to_tree r tree;
    UConsole.print (spf "Note: checking out '%s'." str);
    UConsole.print ("You are in 'detached HEAD' state");
  (*e: [[Cmd_checkout.checkout()]] cases *)
  | _ -> raise Cmd.ShowUsage
(*e: function [[Cmd_checkout.checkout]] *)

(*s: function [[Cmd_checkout.update]] *)
(* Your branch is up-to-date with 'origin/master'. *)
let update _r =
  raise Todo
(*e: function [[Cmd_checkout.update]] *)

(*s: constant [[Cmd_checkout.cmd]] *)
let cmd = { Cmd.
  name = "checkout";
  usage = " [options] <branch>
   or: ocamlgit checkout [options] <commitid>
   or: ocamlgit checkout [options]
";
  options = [
   (*s: [[Cmd_checkout.cmd]] command-line options *)
    (* less: --detach, --patch?
     * -b create and checkout a branch
     *)
   (*e: [[Cmd_checkout.cmd]] command-line options *)
  ];
  f = (fun args ->
    let r, _ = Repository.find_root_open_and_adjust_paths [] in
    match args with
    (*s: [[Cmd_checkout.cmd]] match args cases *)
    | [str] -> checkout r str
    (*x: [[Cmd_checkout.cmd]] match args cases *)
    | [] -> update r
    (*e: [[Cmd_checkout.cmd]] match args cases *)
    | _ -> raise Cmd.ShowUsage
  );
}
(*e: constant [[Cmd_checkout.cmd]] *)
(*e: version_control/cmd_checkout.ml *)
