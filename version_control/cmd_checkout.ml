(* Copyright 2017 Yoann Padioleau, see copyright.txt *)
open Common

let checkout r str =
  let all_refs = Repository.all_refs r in
  let refname = "refs/heads/" ^ str in
  if List.mem refname all_refs
  then begin
    let commitid = Repository.follow_ref_some r (Refs.Ref refname) in
    let commit = Repository.read_commit r commitid in
    let treeid = commit.Commit.tree in
    let tree = Repository.read_tree r treeid in
    (* todo: order of operation? set ref before index? reverse? *)
    Repository.set_worktree_and_index_to_tree r tree;
    Repository.write_ref r (Refs.Head) (Refs.OtherRef refname);
  end else 
    (* checkout an sha? detached head? *)
    raise Todo

(* Your branch is up-to-date with 'origin/master'. *)
let update r =
  raise Todo

let cmd = { Cmd.
  name = "checkout";
  help = "";
  options = [];
  f = (fun args ->
    (* todo: allow git rm from different location *)
    let r = Repository.open_ "." in
    match args with
    | [] -> update r
    | [str] -> checkout r str
    | _ -> raise Cmd.ShowUsage
  );
}
