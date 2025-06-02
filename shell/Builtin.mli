(*s: Builtin.mli *)
(*s: signature [[Builtin.is_builtin]] *)
val is_builtin : string -> bool
(*e: signature [[Builtin.is_builtin]] *)

(*s: signature [[Builtin.dispatch]] *)
(* execute the builtin *)
val dispatch : < Cap.chdir ; Cap.exit ; Cap.open_in; .. > -> string -> unit
(*e: signature [[Builtin.dispatch]] *)
(*e: Builtin.mli *)
