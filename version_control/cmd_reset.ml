(*s: version_control/cmd_reset.ml *)
(*s: copyright ocamlgit *)
(* Copyright 2017 Yoann Padioleau, see copyright.txt *)
(*e: copyright ocamlgit *)
open Common

(*s: function Cmd_reset.reset_hard *)
let reset_hard r =
  let commitid = Repository.follow_ref_some r (Refs.Head) in
  let commit = Repository.read_commit r commitid in
  let treeid = commit.Commit.tree in
  let tree = Repository.read_tree r treeid in
  Repository.set_worktree_and_index_to_tree r tree;
  pr (spf "HEAD is now at %s %s" 
        (String.sub (Hexsha.of_sha commitid) 0 6)
        (String.sub commit.Commit.message 0 40))
(*e: function Cmd_reset.reset_hard *)

(*s: constant Cmd_reset.hard *)
let hard = ref false
(*e: constant Cmd_reset.hard *)
(*s: constant Cmd_reset.soft *)
let soft = ref false
(*e: constant Cmd_reset.soft *)
(*s: constant Cmd_reset.mixed *)
let mixed = ref false
(*e: constant Cmd_reset.mixed *)

(*s: constant Cmd_reset.cmd *)
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
    let r, _ = Repository.find_root_open_and_adjust_paths [] in
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
(*e: constant Cmd_reset.cmd *)
(*e: version_control/cmd_reset.ml *)
