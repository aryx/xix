(*s: version_control/cmd_init.ml *)
(*s: copyright gut *)
(* Copyright 2017 Yoann Padioleau, see copyright.txt *)
(*e: copyright gut *)
open Common

(* less: let bare = ref false *)

(*s: constant Cmd_init.cmd *)
let cmd = { Cmd.
  name = "init";
  help = " [directory]";
  options = [
   (* less: -bare, --quiet *)
  ];
  f = (fun args ->
    match args with
    | [] ->
      Repository.init "."
    | [dir] ->
      Repository.init dir
    | _ -> raise Cmd.ShowUsage
  );
}
(*e: constant Cmd_init.cmd *)
(*e: version_control/cmd_init.ml *)
