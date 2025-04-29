
val is_builtin: string -> bool

(* execute the builtin *)
val dispatch: 
  < Cap.chdir; Cap.exit; .. > ->
  string -> unit
