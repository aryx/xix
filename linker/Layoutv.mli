(* Returns symbol_table2 with SData2 and SBss2 entries populated.
 * Returns also data_size x bss_size.
 *)
val layout_data: 
  Types.symbol_table -> Types.data list -> 
  Types.symbol_table2 * (int * int)

(* Returns symbol_table2 with SText2 entries populated. 
 * Returns also nodes in code_graph with their real_pc field set.
 * Returns also text_size.
 * !! works by side effect on code_graph and symbol_table2, so take care !!
 *)
val layout_text: 
  Types.symbol_table2 -> Types.real_pc (* INITTEXT *) -> Typesv.code_graph -> 
  Types.symbol_table2 * Typesv.code_graph * int

