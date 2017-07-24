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
  raise Todo

let delete_branch r name =
  raise Todo

let cmd = { Cmd.
  name = "branch";
  help = "";
  options = [];
  f = (fun args ->
    (* todo: allow git rm from different location *)
    let r = Repository.open_ "." in
    match args with
    | [] -> 
      list_branches r
    | [name] ->
      create_branch r name
    | [name;objectish] ->
      raise Todo
    | _ -> raise Cmd.ShowUsage
  );
}
