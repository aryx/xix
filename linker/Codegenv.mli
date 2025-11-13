(* This is used for the code layout. *)
val size_of_instruction: 
  Codegen.env -> Ast_asmv.instr Types.node -> 
  int (* a multiple of 4 *) * Codegen.pool option


(* uses only config.init_text and for sanity checking only *)
val gen: 
  Types.symbol_table2 -> Exec_file.linker_config ->
  Ast_asmv.instr Types.code_graph -> 
  Types.word list
