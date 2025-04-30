val is_builtin : string -> bool

(* execute the builtin *)
val dispatch : < Cap.chdir ; Cap.exit ; Cap.open_in; .. > -> string -> unit
