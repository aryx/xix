(* Copyright 2017 Yoann Padioleau, see copyright.txt *)
open Common

type status = {
  (* diff index vs HEAD *)
  staged: Change.t list;
  (* diff worktree vs index *)
  unstaged: Change.t list;
  (* other *)
  untracked: Common.filename list;
}

(* TODO *)
let status_of_repository r =
  { staged = [];
    unstaged = [];
    untracked = [];
  }


let short_format = ref false

let status r =
  let _st = status_of_repository r in
  
  raise Todo

let cmd = { Cmd.
  name = "status";
  help = " [options]"; (* less: <pathspec> *)
  options = [
    "--short", Arg.Set short_format, " show status concisely";
    "--long", Arg.Clear short_format, " show status in long format (default)";
    (* less: --branch, --ignored *)
  ];
  f = (fun args ->
    (* todo: allow git rm from different location *)
    let r = Repository.open_ "." in
    match args with
    | [] -> status r
    | xs -> raise Cmd.ShowUsage
  );
}
