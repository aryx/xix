(*s: Codegen5.mli *)

(*s: type [[Codegen5.pool (Codegen5.mli)]] *)
type pool =
  (* note that it is not always an int! Sometimes it can be an
   * Address which will be resolved only at the very end.
   *)
  | PoolOperand of Ast_asm.ximm
  (* todo: still don't know why we need that *)
  | LPOOL 
(*e: type [[Codegen5.pool (Codegen5.mli)]] *)

(*s: signature [[Codegen5.size_of_instruction]] *)
(* This is used for the code layout. *)
val size_of_instruction: 
  Codegen.env -> Types5.node -> int (* a multiple of 4 *) * pool option
(*e: signature [[Codegen5.size_of_instruction]] *)

(*s: signature [[Codegen5.gen]] *)
(* uses only config.init_text and for sanity checking only *)
val gen: 
  Types.symbol_table2 -> Exec_file.linker_config -> Types5.code_graph -> 
  Types.word list
(*e: signature [[Codegen5.gen]] *)

(* internals *)
val rules: Codegen.env -> Types.addr option -> Types5.node -> pool Codegen.action

(*e: Codegen5.mli *)
