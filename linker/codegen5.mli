
(* This is used for the code layout *)
val size_of_instruction: 
  Types.symbol_table2 -> Types5.node -> int

val gen: 
  Types.symbol_table2 -> Types.config -> Types5.code_graph -> Types.word list
