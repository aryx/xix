(* Copyright 2017 Yoann Padioleau, see copyright.txt *)
open Common

let list_extra = ref false

let rec cmd = { Cmd.
  name = "help";
  help = "";
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
