
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
]

let extra_commands = [
  Cmd_test.cmd;
  Cmd_dump.cmd;
]
