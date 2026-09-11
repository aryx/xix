(* Returns symbol_table2 with SText2 entries populated.
 * Returns also nodes in code_graph with their real_pc field set.
 * Returns also text_size.
 * !! works by side effect on code_graph and symbol_table2, so take care !!
 *)
val layout_text:
  Types.symbol_table2 -> Types.real_pc (* INITTEXT *) ->
  Ast_asmi.instr Types.code_graph ->
  Types.symbol_table2 * Ast_asmi.instr Types.code_graph * int
