(*s: version_control/cmd_add.ml *)
(*s: copyright ocamlgit *)
(* Copyright 2017 Yoann Padioleau, see copyright.txt *)
(*e: copyright ocamlgit *)
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
     * --all, 
     * --recursive
     *)
  ];
  f = (fun args ->
    match args with
    | [] -> pr2 "Nothing specified, nothing added."
    | xs ->
      let r, relpaths = Repository.find_root_open_and_adjust_paths xs in
      (* less: support directories *)
      add r relpaths
  );
}
(*e: constant Cmd_add.cmd *)
(*e: version_control/cmd_add.ml *)
