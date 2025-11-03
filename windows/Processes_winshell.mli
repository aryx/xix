(*s: Processes_winshell.mli *)

(*s: signature [[Processes_winshell.run_cmd_in_window_in_child_of_fork]] *)
val run_cmd_in_window_in_child_of_fork:
  < Cap.chdir; Cap.exec; Cap.mount; Cap.bind; .. > ->
  string -> string array -> Window.t -> Fileserver.t -> unit
(*e: signature [[Processes_winshell.run_cmd_in_window_in_child_of_fork]] *)
(*e: Processes_winshell.mli *)
