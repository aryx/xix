
(* uses only sizes.data_size *)
val gen: 
  Types.symbol_table2 -> Types.addr (* init_data *) -> Types.sections_size ->
  Types.data list ->
  Types.byte array
