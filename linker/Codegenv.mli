type pool =
  | PoolOperand of Ast_asm.ximm
  | LPOOL

(* This is used for the code layout. *)
val size_of_instruction: 
  Types.symbol_table2 -> int (* autosize*) -> Typesv.node -> 
  int (* a multiple of 4 *) * pool option


(* uses only config.init_text and for sanity checking only *)
val gen: 
  Types.symbol_table2 -> Exec_file.linker_config -> Typesv.code_graph -> 
  Types.word list
