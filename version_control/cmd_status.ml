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

(* some commonalities with Repository.set_worktree_and_index_to_tree *)
let changes_index_vs_HEAD r =
  let commitid = Repository.follow_ref_some r (Refs.Head) in
  let commit = Repository.read_commit r commitid in
  let treeid = commit.Commit.tree in
  let tree = Repository.read_tree r treeid in

  let h_in_index_and_head = Hashtbl.create 101 in
  let hindex = 
    r.Repository.index 
    |> List.map (fun entry -> entry.Index.name, entry)
    |> Hashtbl_.of_list
  in
  let changes = ref [] in

  tree |> Tree.walk_tree (Repository.read_tree r) "" (fun relpath entry_head ->
    let perm = entry_head.Tree.perm in
    match perm with
    | Tree.Dir -> ()
    | Tree.Commit -> failwith "submodule not yet supported"
    | Tree.Normal | Tree.Exec | Tree.Link ->
      try
        let entry_index = Hashtbl.find hindex relpath in
        Hashtbl.add h_in_index_and_head relpath true;
        (* less: if change mode, then report as del/add *)
        if entry_head.Tree.node <> entry_index.Index.id
        then changes |> Common.push (Change.Modify (
          { Change.path = relpath; 
            mode = Index.mode_of_perm perm; 
            content = lazy (raise (Impossible "not called")); },
          { Change.path = relpath; 
            mode = entry_index.Index.stats.Index.mode;
            content = lazy (raise (Impossible "not called")); }
        ))
      with Not_found ->
        changes |> Common.push (Change.Del { Change.
             path = relpath;
             mode = Index.mode_of_perm perm;
             content = lazy (raise (Impossible "not called"));
                                           });
  );
  r.Repository.index |> List.iter (fun entry ->
    if not (Hashtbl.mem h_in_index_and_head entry.Index.name)
    then changes |> Common.push (Change.Add { Change.
             path = entry.Index.name;
             mode = entry.Index.stats.Index.mode;                                            content = lazy (raise (Impossible "not called")); }
    )
  );
  (* less: sort by path *)
  List.rev !changes

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


let status_of_repository r =
  { staged = changes_index_vs_HEAD r;
    unstaged = Cmd_diff.changes_worktree_vs_index r;
    untracked = untracked r;
  }

(* very similar to Cmd_log.print_change, but with more indentation *)
let print_change_long change =
  match change with
  | Change.Add entry ->
    pr (spf "	new file:	%s" entry.Change.path)
  | Change.Del entry ->
    pr (spf "	deleted:	%s" entry.Change.path)
  | Change.Modify (entry1, entry2) ->
    pr (spf "	modified:	%s" entry1.Change.path)


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
    

let print_status_short st =
  raise Todo

let short_format = ref false

let status r =
  let st = status_of_repository r in
  if !short_format
  then print_status_short st
  else print_status_long st

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
