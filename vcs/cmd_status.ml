(*s: version_control/cmd_status.ml *)
(*s: copyright ocamlgit *)
(* Copyright 2017 Yoann Padioleau, see copyright.txt *)
(*e: copyright ocamlgit *)
open Common
open Fpath_.Operators
open Regexp_.Operators

(*s: type [[Cmd_status.status]] *)
type status = {
  (* diff index vs HEAD *)
  staged: Change.t list;
  (* diff worktree vs index *)
  unstaged: Change.t list;
  (* other *)
  untracked: Fpath.t list;
}
(*e: type [[Cmd_status.status]] *)

(*s: function [[Cmd_status.changes_index_vs_HEAD]] *)
let changes_index_vs_HEAD caps r =
  let commitid = Repository.follow_ref_some caps r (Refs.Head) in
  let commit = Repository.read_commit caps r commitid in
  let treeid = commit.Commit.tree in
  Changes.changes_index_vs_tree (Repository.read_tree caps r) 
    r.Repository.index
    treeid
(*e: function [[Cmd_status.changes_index_vs_HEAD]] *)

(*s: function [[Cmd_status.untracked]] *)
(* todo: need parse .gitignore *)
let untracked r =
  let h = r.Repository.index 
      |> List.map (fun entry -> entry.Index.path, true) 
      |> Hashtbl_.of_list 
  in
  let res = ref [] in
  r.Repository.worktree |> Repository.walk_dir (fun dir _dirs files ->
    files |> List.iter (fun file ->
      let path = dir / file in
      let path = 
        if !!path =~ "^\\./\\(.*\\)"
        then Fpath.v (Regexp_.matched1 !!path)
        else path
      in
      if not (Hashtbl.mem h path)
      then Stack_.push path res
    );
  );
  List.rev !res
(*e: function [[Cmd_status.untracked]] *)


(*s: function [[Cmd_status.status_of_repository]] *)
let status_of_repository caps (r : Repository.t) =
  { staged = changes_index_vs_HEAD caps r;
    unstaged = 
      Changes.changes_worktree_vs_index caps
        (Repository.read_blob caps r)
        r.worktree 
        r.index;
    untracked = untracked r;
  }
(*e: function [[Cmd_status.status_of_repository]] *)

(*s: function [[Cmd_status.print_change_long]] *)
(* very similar to Cmd_log.print_change, but with more indentation *)
let print_change_long (caps : < Cap.stdout; ..>) change =
  match change with
  | Change.Add entry ->
    Console.print caps (spf "	new file:	%s" !!(entry.Change.path))
  | Change.Del entry ->
    Console.print caps (spf "	deleted:	%s" !!(entry.Change.path))
  | Change.Modify (entry1, _entry2) ->
    Console.print caps (spf "	modified:	%s" !!(entry1.Change.path))
(*e: function [[Cmd_status.print_change_long]] *)


(*s: function [[Cmd_status.print_status_long]] *)
let print_status_long (caps : <Cap.stdout; ..>) st =
  if st.staged <> []
  then begin
    Console.print caps "Changes to be committed:";
(*  (use "git reset HEAD <file>..." to unstage) *)
    Console.print caps "";
    st.staged |> List.iter (print_change_long caps);
    Console.print caps "";
  end;
  if st.unstaged <> []
  then begin
    Console.print caps "Changes not staged for commit:";
    Console.print caps "";
(*
  (use "git add/rm <file>..." to update what will be committed)
  (use "git checkout -- <file>..." to discard changes in working directory)
*)

    st.unstaged |> List.iter (print_change_long caps);
    Console.print caps "";
  end;
  if st.untracked <> []
  then begin
    Console.print caps "Untracked files:";
(*  (use "git add <file>..." to include in what will be committed) *)
    Console.print caps "";
    st.untracked |> List.iter (fun file ->
      Console.print caps (spf "	%s" !!file)
    );
    Console.print caps "";
  end
(*e: function [[Cmd_status.print_status_long]] *)
    

(*s: function [[Cmd_status.print_status_short]] *)
let print_status_short _st =
  raise Todo
(*e: function [[Cmd_status.print_status_short]] *)

(*s: constant [[Cmd_status.short_format]] *)
let short_format = ref false
(*e: constant [[Cmd_status.short_format]] *)

(*s: function [[Cmd_status.status]] *)
let status caps r =
  let st = status_of_repository caps r in
  if !short_format
  then print_status_short st
  else print_status_long caps st
(*e: function [[Cmd_status.status]] *)

(*s: constant [[Cmd_status.cmd]] *)
let cmd = { Cmd_.
  name = "status";
  usage = " [options]"; (* less: <pathspec> *)
  options = [
    "--short", Arg.Set short_format, " show status concisely";
    "--long", Arg.Clear short_format, " show status in long format (default)";
    (* less: --branch, --ignored *)
  ];
  f = (fun caps args ->
    let r, relpaths = 
          Repository.find_root_open_and_adjust_paths caps
            (Fpath_.of_strings args) in
    match relpaths with
    | [] -> status caps r
    | _xs -> raise Cmd_.ShowUsage
  );
}
(*e: constant [[Cmd_status.cmd]] *)
(*e: version_control/cmd_status.ml *)
