
val run_cmd_in_window_in_child_of_fork:
  < Cap.chdir; Cap.exec; Cap.mount; Cap.bind; .. > ->
  string -> string array -> Window.t -> Fileserver.t -> unit
