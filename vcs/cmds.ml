(*s: version_control/cmds.ml *)

(*s: constant [[Cmds.main_commands]] *)
let main_commands = [
  (* creating *)
  Cmd_init.cmd;
  Cmd_add.cmd;
  Cmd_rm.cmd;
  Cmd_commit.cmd;

  (* branching *)
  Cmd_branch.cmd;
  Cmd_checkout.cmd;
  Cmd_reset.cmd;
  
  (* inspecting *)
  Cmd_show.cmd;
  Cmd_diff.cmd;
  Cmd_log.cmd;
  Cmd_status.cmd;

  (* networking *)
  Cmd_pull.cmd;
  Cmd_push.cmd;
  Cmd_clone.cmd;
]
(*e: constant [[Cmds.main_commands]] *)

(*s: constant [[Cmds.extra_commands]] *)
let extra_commands = [
  Cmd_test.cmd;
  Cmd_dump.cmd;
]
(*e: constant [[Cmds.extra_commands]] *)
(*e: version_control/cmds.ml *)
