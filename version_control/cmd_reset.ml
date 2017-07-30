(* Copyright 2017 Yoann Padioleau, see copyright.txt *)
open Common

let reset_hard r =
  let commitid = Repository.follow_ref_some r (Refs.Head) in
  let commit = Repository.read_commit r commitid in
  let treeid = commit.Commit.tree in
  let tree = Repository.read_tree r treeid in
  Repository.set_worktree_and_index_to_tree r tree;
  pr (spf "HEAD is now at %s %s" 
        (String.sub (Hexsha.of_sha commitid) 0 6)
        (String.sub commit.Commit.message 0 40))

let hard = ref false
let soft = ref false
let mixed = ref false

let cmd = { Cmd.
  name = "reset";
  help = " [options] ";
(* less: or: git reset <paths>... *)
  options = [
    "--hard", Arg.Set hard, " reset HEAD, index and working tree";
    "--soft", Arg.Set soft, " reset only HEAD";
    "--mixed", Arg.Set mixed, " reset HEAD and index";
    (* less: --patch, --quiet, --merge *)
  ];
  f = (fun args ->
    (* todo: allow git rm from different location *)
    let r = Repository.open_ "." in
    match args with
    | [] -> 
      if !soft || !mixed || not !hard
      then begin
        pr2 "only --hard supported";
        raise Cmd.ShowUsage
      end;
      reset_hard r
    | _ -> raise Cmd.ShowUsage
  );
}
