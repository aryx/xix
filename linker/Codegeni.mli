(* BIG bias for RSB (see Codegeni.ml); needed by CLI.ml's linki to
 * xdefine "setSB" with the same value goken's il does. *)
val big : int

(* This is used for the code layout. *)
val size_of_instruction:
  Codegen.env -> Ast_asmi.instr Types.node -> int (* a multiple of 4 *)

(* uses only config.init_text and for sanity checking only *)
val gen:
  Types.symbol_table2 -> Exec_file.linker_config ->
  Ast_asmi.instr Types.code_graph ->
  Types.word list
