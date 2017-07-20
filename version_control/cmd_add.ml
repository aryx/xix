(* Copyright 2017 Yoann Padioleau, see copyright.txt *)
open Common

let rec cmd = { Cmd.
  name = "add";
  help = "";
  options = [];
  f = (fun args ->
    match args with
    | [] -> failwith "Nothing specified, nothing added."
    | xs ->
      (* todo: allow git add from different location *)
      let r = Repository.open_ "." in
      (* less: support absolute paths, directories *)
      let relatives =
        raise Todo
      in
      
      raise Todo
  );
}
