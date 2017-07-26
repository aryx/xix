(* Copyright 2017 Yoann Padioleau, see copyright.txt *)
open Common

let show r objectish =
  let obj = Repository.read_objectish r objectish in
  match obj with
  | Objects.Blob x -> 
    Blob.show x
  | Objects.Tree x ->
    (* =~ git ls-tree --names-only *)
    pr "tree\n"; (* less: put sha of tree *)
    Tree.show x
  | Objects.Commit x -> 
    pr "commit"; (* less: put sha of commit *)
    Commit.show x

let cmd = { Cmd.
  name = "show";
  help = " <objectish>";
  (* less: --oneline *)
  options = [];
  f = (fun args ->
    (* todo: allow git rm from different location *)
    let r = Repository.open_ "." in
    match args with
    | [] -> show r (Repository.ObjByRef (Refs.Head))
    | xs ->
      xs |> List.iter (fun str ->
        show r (Repository.ObjByHex (str))
      )
  );
}
