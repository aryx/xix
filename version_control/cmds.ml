
let main_commands = [
  Cmd_init.cmd;
  Cmd_add.cmd;
  Cmd_rm.cmd;
  Cmd_commit.cmd;

  Cmd_branch.cmd;
  Cmd_checkout.cmd;
  Cmd_reset.cmd;
  
  Cmd_show.cmd;
  Cmd_diff.cmd;
  Cmd_log.cmd;
  Cmd_status.cmd;

  Cmd_pull.cmd;
  Cmd_clone.cmd;
]

let extra_commands = [
  Cmd_test.cmd;
  Cmd_dump.cmd;
]
