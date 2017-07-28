(* Copyright 2017 Yoann Padioleau, see copyright.txt *)
open Common

let add r relpaths = 
  (* this will also add some blobs to the object store *)
  Repository.add_in_index r relpaths

let cmd = { Cmd.
  name = "add";
  help = " <file>..."; (* less: pathspec? *)
  options = [
    (* todo: --interactive, --patch for picking *)
  ];
  f = (fun args ->
    match args with
    | [] -> pr2 "Nothing specified, nothing added."
    | xs ->
      (* todo: allow git add from different location *)
      let r = Repository.open_ "." in
      (* less: support absolute paths, directories *)
      let relpaths = xs |> List.map (fun path ->
        if Filename.is_relative path
        then path
        else failwith (spf "Not a relative path: %s" path)
      )
      in
      add r relpaths
  );
}
