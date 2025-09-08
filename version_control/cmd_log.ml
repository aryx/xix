(*s: version_control/cmd_log.ml *)
(*s: copyright ocamlgit *)
(* Copyright 2017 Yoann Padioleau, see copyright.txt *)
(*e: copyright ocamlgit *)
open Common

(* todo: git log --graph --oneline --decorate --all *)

(*s: function [[Cmd_log.print_commit]] *)
let print_commit sha commit =
  UConsole.print (spf "commit: %s" (Hexsha.of_sha sha));
  (match commit.Commit.parents with
  | [] | [_] -> ()
  | _x::xs ->
    UConsole.print (spf "merge: %s" 
          (xs |> List.map Hexsha.of_sha |> String.concat "..."));
  );
  let author = commit.Commit.author in
  UConsole.print (spf "Author: %s <%s>" author.User.name author.User.email);
  let committer = commit.Commit.committer in
  if author <> committer
  then 
    UConsole.print (spf "Committer: %s <%s>" committer.User.name committer.User.email);
  UConsole.print (spf "Date:   %s" (User.string_of_date author.User.date));
  UConsole.print "";
  UConsole.print ("    " ^ commit.Commit.message);
  ()
(*e: function [[Cmd_log.print_commit]] *)

(*s: function [[Cmd_log.print_change]] *)
let print_change change =
  match change with
  | Change.Add entry ->
    UConsole.print (spf "A       %s" entry.Change.path)
  | Change.Del entry ->
    UConsole.print (spf "D       %s" entry.Change.path)
  | Change.Modify (entry1, _entry2) ->
    UConsole.print (spf "M       %s" entry1.Change.path)
(*e: function [[Cmd_log.print_change]] *)


(*s: constant [[Cmd_log.name_status]] *)
let name_status = ref false
(*e: constant [[Cmd_log.name_status]] *)

(*s: function [[Cmd_log.log]] *)
(* todo: track only selected paths 
 * (and then rename detection to track correctly)
 *)
let log r =
  let start = Repository.follow_ref_some r (Refs.Head) in
  start |> Commit.walk_history (Repository.read_commit r) (fun sha commit ->
    print_commit sha commit;
    (*s: [[Cmd_log.log()]] if [[--name-status]] flag *)
    if !name_status
    then begin
      let tree1 = Repository.read_tree r commit.Commit.tree in
      let tree2 =
        match commit.Commit.parents with
        | [] -> []
        | [sha] -> 
          let commit2 = Repository.read_commit r sha in
          Repository.read_tree r commit2.Commit.tree
        | _x::_y::_xs ->
          failwith "TODO: log: handle merge"
      in
      let changes = Changes.changes_tree_vs_tree
        (Repository.read_tree r)
        (Repository.read_blob r)
        tree2
        tree1
      in
      changes |> List.iter print_change;
      UConsole.print "";
    (*e: [[Cmd_log.log()]] if [[--name-status]] flag *)
    end
  )
(*e: function [[Cmd_log.log]] *)

(*s: constant [[Cmd_log.cmd]] *)
let cmd = { Cmd_.
  name = "log";
  usage = " [options]";
  options = [
    "--name-status", Arg.Set name_status, 
    " print name/status for each changed file";
    (* todo: -1, -10 *)
    (* less: --reverse *)
  ];
  f = (fun args ->
    let r, relpaths = Repository.find_root_open_and_adjust_paths args in
    match relpaths with
    | [] -> log r
    (* todo: git log path *)
    (* less: revision range *)
    | _xs -> raise Cmd_.ShowUsage
  );
}
(*e: constant [[Cmd_log.cmd]] *)
(*e: version_control/cmd_log.ml *)
