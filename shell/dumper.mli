
val s_of_cmd:
  Ast.cmd -> string

val s_of_opcode:
  Opcode.opcode -> string

(* extra *)

val s_of_line: 
  Ast.line -> string
val s_of_cmd_sequence:
  Ast.cmd_sequence -> string

val s_of_codevec:
  Opcode.codevec -> string
