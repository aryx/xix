(*s: version_control/cmd_help.ml *)
(*s: copyright ocamlgit *)
(* Copyright 2017 Yoann Padioleau, see copyright.txt *)
(*e: copyright ocamlgit *)
open Common

(*s: constant [[Cmd_help.list_extra]] *)
let list_extra = ref false
(*e: constant [[Cmd_help.list_extra]] *)

(*s: constant [[Cmd_help.cmd]] *)
let rec cmd = { Cmd.
  name = "help";
  usage = "";
  options = ["-a", Arg.Set list_extra, " see all commands"];
  f = (fun args ->
    let xs = 
      if !list_extra
      then Cmds.main_commands @ Cmds.extra_commands @ [cmd]
      else Cmds.main_commands
    in
    pr ("Available commands: ");
    xs |> List.iter (fun cmd ->
      pr (spf "  %s" cmd.Cmd.name);
    );
  );
}
(*e: constant [[Cmd_help.cmd]] *)
(*e: version_control/cmd_help.ml *)
