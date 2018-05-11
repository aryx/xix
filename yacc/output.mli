(*s: yacc/output.mli *)

(*s: signature [[Output.output_parser]](yacc) *)
val output_parser: 
  Ast.parser_definition -> Lr0.env -> Lrtables.lr_tables -> 
  in_channel -> out_channel -> unit
(*e: signature [[Output.output_parser]](yacc) *)
(*e: yacc/output.mli *)
