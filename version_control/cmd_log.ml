(* Copyright 2017 Yoann Padioleau, see copyright.txt *)
open Common

let name_status = ref false

let print_commit sha commit =
  pr (spf "commit: %s" (Hexsha.of_sha sha));
  (match commit.Commit.parents with
  | [] | [_] -> ()
  | x::xs ->
    pr (spf "merge: %s" 
          (xs |> List.map Hexsha.of_sha |> String.concat "..."));
  );
  let author = commit.Commit.author in
  pr (spf "Author: %s <%s>" author.User.name author.User.email);
  let committer = commit.Commit.committer in
  if author <> committer
  then 
    pr (spf "Committer: %s <%s>" committer.User.name committer.User.email);
  pr (spf "Date:   %s" (User.string_of_date author.User.date));
  pr "";
  pr ("    " ^ commit.Commit.message);
  ()

let print_name_status change =
  raise Todo

let rec walk_history r f sha =
  let commit = Repository.read_commit r sha in
  (* todo: path matching *)
  f commit;
  (* naive: but will print duplicate when merge and before branch *)
  commit.Commit.parents |> List.iter (walk_history r f)

let log r =
  let start = Repository.follow_ref_some r (Refs.Head) in
  start |> walk_history r (fun commit ->
    print_commit start commit
  )

let cmd = { Cmd.
  name = "log";
  help = " [options]";
  (* todo: -1, -10 *)
  options = [
    "--name-status", Arg.Set name_status, 
    " print name/status for each changed file";
  ];
  f = (fun args ->
    (* todo: allow git rm from different location *)
    let r = Repository.open_ "." in
    match args with
    | [] -> log r
    (* todo: git log path *)
    (* less: revision range *)
    | xs -> raise Cmd.ShowUsage
  );
}
