(*s: Parse.mli *)
(*s: signature [[Parse.parse]] *)
(* !internally modifies Globals.line and Globals.file! *)
val parse : Fpath.t -> Ast.instr list
(*e: signature [[Parse.parse]] *)
(*e: Parse.mli *)
