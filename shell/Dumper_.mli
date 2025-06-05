(*s: Dumper_.mli *)
(*s: signature [[Dumper_.s_of_cmd]] *)
val s_of_cmd : Ast.cmd -> string
(*e: signature [[Dumper_.s_of_cmd]] *)
(*s: signature [[Dumper_.s_of_opcode]] *)
val s_of_opcode : Opcode.opcode -> string
(*e: signature [[Dumper_.s_of_opcode]] *)

(* extra *)

(*s: signature [[Dumper_.s_of_line]] *)
val s_of_line : Ast.line -> string
(*e: signature [[Dumper_.s_of_line]] *)
(*s: signature [[Dumper_.s_of_cmd_sequence]] *)
val s_of_cmd_sequence : Ast.cmd_sequence -> string
(*e: signature [[Dumper_.s_of_cmd_sequence]] *)
(*s: signature [[Dumper_.s_of_value]] *)
val s_of_value : Ast.value -> string
(*e: signature [[Dumper_.s_of_value]] *)
(*s: signature [[Dumper_.s_of_codevec]] *)
val s_of_codevec : Opcode.codevec -> string
(*e: signature [[Dumper_.s_of_codevec]] *)
(*s: signature [[Dumper_.s_of_operation]] *)
val s_of_operation : Opcode.operation -> string
(*e: signature [[Dumper_.s_of_operation]] *)
(*e: Dumper_.mli *)
