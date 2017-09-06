(*s: version_control/cmd_add.ml *)
(* Copyright 2017 Yoann Padioleau, see copyright.txt *)
open Common

(*s: function Cmd_add.add *)
let add r relpaths = 
  (* this will also add some blobs to the object store *)
  Repository.add_in_index r relpaths
(*e: function Cmd_add.add *)

(*s: constant Cmd_add.cmd *)
let cmd = { Cmd.
  name = "add";
  help = " <file>..."; (* less: pathspec? *)
  options = [
    (* todo: --interactive, --patch for picking, --force (if ignored) 
     * --all
    *)
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
(*e: constant Cmd_add.cmd *)
(*e: version_control/cmd_add.ml *)
