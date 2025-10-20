
type pool =
  | PoolOperand of Ast_asm.imm_or_ximm
  | LPOOL

(* This is used for the code layout. *)
val size_of_instruction: 
  Types.symbol_table2 -> int (* autosize*) -> Types5.node -> 
  int (* a multiple of 4 *) * pool option

(* uses only config.init_text and for sanity checking only *)
val gen: 
  Types.symbol_table2 -> Types.config -> Types5.code_graph -> 
  Types.word list

