(*s: version_control/cmd_init.ml *)
(*s: copyright ocamlgit *)
(* Copyright 2017 Yoann Padioleau, see copyright.txt *)
(*e: copyright ocamlgit *)
open Common

(* less: let bare = ref false *)

(*s: constant Cmd_init.cmd *)
let cmd = { Cmd.
  name = "init";
  usage = " [directory]";
  options = [
   (*s: [[Cmd_init.cmd]] command-line options *)
   (* less: -bare, --quiet *)
   (*e: [[Cmd_init.cmd]] command-line options *)
  ];
  f = (fun args ->
    match args with
    | []    -> Repository.init "."
    | [dir] -> Repository.init dir
    | _ -> raise Cmd.ShowUsage
  );
}
(*e: constant Cmd_init.cmd *)
(*e: version_control/cmd_init.ml *)
