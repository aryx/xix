
val build_graph:
  Types.symbol_table -> Types5.code array -> Types5.code_graph

(* returns also data_size x bss_size *)
val layout_data: 
  Types.symbol_table -> Types.symbol_table2 * (int * int)

(* returns also text_size *)
val layout_text: 
  Types.symbol_table2 -> Types.real_pc (* INITTEXT *) -> Types5.code_graph -> 
  Types.symbol_table2 * int
