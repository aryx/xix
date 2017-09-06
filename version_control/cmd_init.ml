(*s: version_control/cmd_init.ml *)
(* Copyright 2017 Yoann Padioleau, see copyright.txt *)
open Common

(*s: constant Cmd_init.cmd *)
(* less: let bare = ref false *)

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
