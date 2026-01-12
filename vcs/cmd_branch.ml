(*s: version_control/cmd_branch.ml *)
(*s: copyright ocamlgit *)
(* Copyright 2017 Yoann Padioleau, see copyright.txt *)
(*e: copyright ocamlgit *)
open Common
open Regexp_.Operators


(*s: function [[Cmd_branch.list_branches]] *)
(* less: remote_flag set with --all to also list remote refs *)
let list_branches (caps : < Cap.stdout; ..>) r =
  let head_branch = Repository.read_ref r (Refs.Head) in
  let all_refs = Repository.all_refs r in
  all_refs |> List.iter (fun refname ->
    if refname =~ "^refs/heads/\\(.*\\)"
    then 
      let short = Regexp_.matched1 refname in
      let prefix = 
        if (Refs.OtherRef refname = head_branch)
        then "* "
        else "  "
      in
      Console.print caps (spf "%s%s" prefix short)
  )
(*e: function [[Cmd_branch.list_branches]] *)

(*s: function [[Cmd_branch.create_branch]] *)
let create_branch r name (* sha *) =
  let refname = "refs/heads/" ^ name in
  (*s: [[Cmd_branch.create_branch()]] sanity check refname *)
  let all_refs = Repository.all_refs r in
  if List.mem refname all_refs
  (* less: unless -force *)
  then failwith (spf "A branch named '%s' already exists." name);
  (*e: [[Cmd_branch.create_branch()]] sanity check refname *)
  let sha = Repository.follow_ref_some r (Refs.Head) in
  let ok = Repository.add_ref_if_new r (Refs.Ref refname) (Refs.Hash sha) in
  if not ok
  then failwith (spf "could not create branch '%s'" name)
(*e: function [[Cmd_branch.create_branch]] *)

(*s: function [[Cmd_branch.delete_branch]] *)
let delete_branch (caps: < Cap.stdout; Cap.open_out; ..>) r name force =
  let refname = "refs/heads/" ^ name in
  let aref = Refs.Ref refname in
  let sha = Repository.follow_ref_some r aref in
  (*s: [[Cmd_branch.delete_branch()]] sanity check if branch merged unless force *)
  if not force
  (* todo: detect if fully merged branch! *)    
  then ();
  (*e: [[Cmd_branch.delete_branch()]] sanity check if branch merged unless force *)
  Repository.del_ref caps r aref;
  Console.print caps (spf "Deleted branch %s (was %s)" name (Hexsha.of_sha sha))
(*e: function [[Cmd_branch.delete_branch]] *)

(* less: rename_branch *)

(*s: constant [[Cmd_branch.del_flag]] *)
let del_flag = ref false
(*e: constant [[Cmd_branch.del_flag]] *)
(*s: constant [[Cmd_branch.del_force]] *)
let del_force = ref false
(*e: constant [[Cmd_branch.del_force]] *)

(*s: constant [[Cmd_branch.cmd]] *)
let cmd = { Cmd_.
  name = "branch";
  usage = " [options]
   or: ocamlgit branch [options] <branchname>
   or: ocamlgit branch [options] (-d | -D) <branchname>
";
  options = [
    (*s: [[Cmd_branch.cmd]] command-line options *)
    (* less: --merged, --no-merged, --all for listing 
     *  --move to rename branch
     *  --force (force creation, deletion, rename)
    *)
    (*x: [[Cmd_branch.cmd]] command-line options *)
    "-d",       Arg.Set del_flag, " delete fully merged branch";
    "--delete", Arg.Set del_flag, " delete fully merged branch";
    (*x: [[Cmd_branch.cmd]] command-line options *)
    "-D",       Arg.Set del_force, " delete branch (even if not merged)";
    (*e: [[Cmd_branch.cmd]] command-line options *)
  ];
  f = (fun caps args ->
    let r, _ = Repository.find_root_open_and_adjust_paths [] in
    match args with
    (*s: [[Cmd_branch.cmd]] match args cases *)
    | [_name;_objectish] ->
      raise Todo
    (*x: [[Cmd_branch.cmd]] match args cases *)
    | [] -> list_branches caps r
    (*x: [[Cmd_branch.cmd]] match args cases *)

    | [name] ->
      (match () with
      (*s: [[Cmd_branch.cmd]] when one argument, when flags cases *)
      | _ when !del_flag  -> delete_branch caps r name false
      (*x: [[Cmd_branch.cmd]] when one argument, when flags cases *)
      | _ when !del_force -> delete_branch caps r name true
      (*e: [[Cmd_branch.cmd]] when one argument, when flags cases *)
      | _ -> create_branch r name
      )
    (*e: [[Cmd_branch.cmd]] match args cases *)
    | _ -> raise Cmd_.ShowUsage
  );
}
(*e: constant [[Cmd_branch.cmd]] *)
(*e: version_control/cmd_branch.ml *)
