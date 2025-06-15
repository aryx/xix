(*s: version_control/cmd_help.ml *)
(*s: copyright ocamlgit *)
(* Copyright 2017 Yoann Padioleau, see copyright.txt *)
(*e: copyright ocamlgit *)
open Common

let (|>) = Stdcompat.(|>)

(*s: constant [[Cmd_help.list_extra]] *)
let list_extra = ref false
(*e: constant [[Cmd_help.list_extra]] *)

(*s: constant [[Cmd_help.cmd]] *)
let rec cmd = { Cmd_.
  name = "help";
  usage = "";
  options = ["-a", Arg.Set list_extra, " see all commands"];
  f = (fun _args ->
    let xs = 
      if !list_extra
      then Cmds.main_commands @ Cmds.extra_commands @ [cmd]
      else Cmds.main_commands
    in
    UConsole.print ("Available commands: ");
    xs |> List.iter (fun cmd ->
      UConsole.print (spf "  %s" cmd.Cmd_.name);
    );
  );
}
(*e: constant [[Cmd_help.cmd]] *)
(*e: version_control/cmd_help.ml *)
