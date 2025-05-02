(*s: version_control/cmd_add.ml *)
(*s: copyright ocamlgit *)
(* Copyright 2017 Yoann Padioleau, see copyright.txt *)
(*e: copyright ocamlgit *)

(*s: function [[Cmd_add.add]] *)
let add r relpaths = 
  (* this will also add some blobs to the object store *)
  Repository.add_in_index r relpaths
(*e: function [[Cmd_add.add]] *)

(*s: constant [[Cmd_add.cmd]] *)
let cmd = { Cmd.
  name = "add";
  usage = " <file>..."; (* less: pathspec? *)
  options = [
    (*s: [[Cmd_add.cmd]] command-line options *)
    (* todo: --interactive, --patch for picking, --force (if ignored) 
     * --all, 
     * --recursive
     *)
    (*e: [[Cmd_add.cmd]] command-line options *)
  ];
  f = (fun args ->
    match args with
    | [] -> Logs.app (fun m -> m "Nothing specified, nothing added.")
    | xs ->
      let r, relpaths = Repository.find_root_open_and_adjust_paths xs in
      (* less: support directories *)
      add r relpaths
  );
}
(*e: constant [[Cmd_add.cmd]] *)
(*e: version_control/cmd_add.ml *)
