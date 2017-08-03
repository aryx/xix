(* Copyright 2017 Yoann Padioleau, see copyright.txt *)
open Common

(* todo: git log --graph --oneline --decorate --all *)

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

let print_change change =
  match change with
  | Change.Add entry ->
    pr (spf "A       %s" entry.Change.path)
  | Change.Del entry ->
    pr (spf "D       %s" entry.Change.path)
  | Change.Modify (entry1, entry2) ->
    pr (spf "M       %s" entry1.Change.path)

(* less: sort by time? so have a sorted queue of commits *)
let walk_history r f sha =
  (* we are walking a DAG, so we need to remember already processed nodes *)
  let hdone = Hashtbl.create 101 in
  let rec aux sha =
    if Hashtbl.mem hdone sha
    then ()
    else begin
      Hashtbl.add hdone sha true;
      let commit = Repository.read_commit r sha in
      (* todo: path matching *)
      f sha commit;
      commit.Commit.parents |> List.iter aux
    end
  in
  aux sha

let name_status = ref false

(* todo: track only selected paths 
 * (and then rename detection to track correctly)
 *)
let log r =
  let start = Repository.follow_ref_some r (Refs.Head) in
  start |> walk_history r (fun sha commit ->
    print_commit sha commit;
    if !name_status
    then begin
      let tree1 = Repository.read_tree r commit.Commit.tree in
      let tree2 =
        match commit.Commit.parents with
        | [] -> []
        | [sha] -> 
          let commit2 = Repository.read_commit r sha in
          Repository.read_tree r commit2.Commit.tree
        | x::y::xs ->
          failwith "TODO: log: handle merge"
      in
      let changes = Diff_tree.changes_tree_vs_tree
        (Repository.read_tree r)
        (Repository.read_blob r)
        tree2
        tree1
      in
      changes |> List.iter print_change;
      pr "";
    end
  )

let cmd = { Cmd.
  name = "log";
  help = " [options]";
  (* todo: -1, -10 *)
  options = [
    "--name-status", Arg.Set name_status, 
    " print name/status for each changed file";
    (* less: --reverse *)
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
