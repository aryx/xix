(* Copyright 2017 Yoann Padioleau, see copyright.txt *)
open Common

let list_branches r =
  let head_branch = Repository.read_ref r (Refs.Head) in
  let all_refs = Repository.all_refs r in
  all_refs |> List.iter (fun refname ->
    if refname =~ "^refs/heads/\\(.*\\)"
    then 
      let short = Regexp_.matched1 refname in
      let prefix = 
        if (Refs.OtherRef refname = head_branch)
        then " * "
        else "   "
      in
      pr (spf "%s%s" prefix short)
  )

let create_branch r name (* sha *) =
  let all_refs = Repository.all_refs r in
  let refname = "refs/heads/" ^ name in
  if List.mem refname all_refs
  (* less: unless -force *)
  then failwith (spf "A branch named '%s' already exists." name);
  let sha = Repository.follow_ref_some r (Refs.Head) in
  let ok = Repository.add_ref_if_new r (Refs.Ref refname) (Refs.Hash sha) in
  if not ok
  then failwith (spf "could not create branch '%s'" name)
    
let delete_branch r name =
  let refname = "refs/heads/" ^ name in
  let aref = Refs.Ref refname in
  let sha = Repository.follow_ref_some r aref in
  Repository.del_ref r aref;
  (* todo: check if still have unmerged commit? *)
  pr (spf "Deleted branch %s (was %s)" name (Hexsha.of_sha sha))


let del_flag = ref false

let cmd = { Cmd.
  name = "branch";
  help = "";
  options = ["-d", Arg.Set del_flag, "delete a branch"];
  f = (fun args ->
    (* todo: allow git rm from different location *)
    let r = Repository.open_ "." in
    match args with
    | [] -> 
      list_branches r
    | [name] ->
      if !del_flag
      then delete_branch r name
      else create_branch r name
    | [name;objectish] ->
      raise Todo
    | _ -> raise Cmd.ShowUsage
  );
}
