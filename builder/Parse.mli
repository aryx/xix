(*s: Parse.mli *)
(*s: signature [[Parse.parse]] *)
(* !internally modifies Globals.line and Globals.file! *)
val parse : Chan.i -> Ast.instr list
(*e: signature [[Parse.parse]] *)
(*e: Parse.mli *)
