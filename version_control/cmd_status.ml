(*s: version_control/cmd_status.ml *)
(*s: copyright gut *)
(* Copyright 2017 Yoann Padioleau, see copyright.txt *)
(*e: copyright gut *)
open Common

(*s: type Cmd_status.status *)
type status = {
  (* diff index vs HEAD *)
  staged: Change.t list;
  (* diff worktree vs index *)
  unstaged: Change.t list;
  (* other *)
  untracked: Common.filename list;
}
(*e: type Cmd_status.status *)

(*s: function Cmd_status.changes_index_vs_HEAD *)
let changes_index_vs_HEAD r =
  let commitid = Repository.follow_ref_some r (Refs.Head) in
  let commit = Repository.read_commit r commitid in
  let treeid = commit.Commit.tree in
  Changes.changes_index_vs_tree (Repository.read_tree r) 
    r.Repository.index
    treeid
(*e: function Cmd_status.changes_index_vs_HEAD *)

(*s: function Cmd_status.untracked *)
(* todo: need parse .gitignore *)
let untracked r =
  let h = r.Repository.index 
      |> List.map (fun entry -> entry.Index.name, true) 
      |> Hashtbl_.of_list 
  in
  let res = ref [] in
  r.Repository.worktree |> Repository.walk_dir (fun dir dirs files ->
    files |> List.iter (fun file ->
      let path = Filename.concat dir file in
      let path = 
        if path =~ "^\\./\\(.*\\)"
        then Regexp_.matched1 path
        else path
      in
      if not (Hashtbl.mem h path)
      then Common.push path res
    );
  );
  List.rev !res
(*e: function Cmd_status.untracked *)


(*s: function Cmd_status.status_of_repository *)
let status_of_repository r =
  { staged = changes_index_vs_HEAD r;
    unstaged = 
      Changes.changes_worktree_vs_index 
        (Repository.read_blob r)
        r.Repository.worktree 
        r.Repository.index;
    untracked = untracked r;
  }
(*e: function Cmd_status.status_of_repository *)

(*s: function Cmd_status.print_change_long *)
(* very similar to Cmd_log.print_change, but with more indentation *)
let print_change_long change =
  match change with
  | Change.Add entry ->
    pr (spf "	new file:	%s" entry.Change.path)
  | Change.Del entry ->
    pr (spf "	deleted:	%s" entry.Change.path)
  | Change.Modify (entry1, entry2) ->
    pr (spf "	modified:	%s" entry1.Change.path)
(*e: function Cmd_status.print_change_long *)


(*s: function Cmd_status.print_status_long *)
let print_status_long st =
  if st.staged <> []
  then begin
    pr "Changes to be committed:";
(*  (use "git reset HEAD <file>..." to unstage) *)
    pr "";
    st.staged |> List.iter print_change_long;
    pr "";
  end;
  if st.unstaged <> []
  then begin
    pr "Changes not staged for commit:";
    pr "";
(*
  (use "git add/rm <file>..." to update what will be committed)
  (use "git checkout -- <file>..." to discard changes in working directory)
*)

    st.unstaged |> List.iter print_change_long;
    pr "";
  end;
  if st.untracked <> []
  then begin
    pr "Untracked files:";
(*  (use "git add <file>..." to include in what will be committed) *)
    pr "";
    st.untracked |> List.iter (fun file ->
      pr (spf "	%s" file)
    );
    pr "";
  end
(*e: function Cmd_status.print_status_long *)
    

(*s: function Cmd_status.print_status_short *)
let print_status_short st =
  raise Todo
(*e: function Cmd_status.print_status_short *)

(*s: constant Cmd_status.short_format *)
let short_format = ref false
(*e: constant Cmd_status.short_format *)

(*s: function Cmd_status.status *)
let status r =
  let st = status_of_repository r in
  if !short_format
  then print_status_short st
  else print_status_long st
(*e: function Cmd_status.status *)

(*s: constant Cmd_status.cmd *)
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
(*e: constant Cmd_status.cmd *)
(*e: version_control/cmd_status.ml *)
