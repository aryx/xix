
(* Returns symbol_table2 with SData2 and SBss2 entries populated.
 * Returns also data_size x bss_size.
 *)
val layout_data: 
  Types.symbol_table -> Types.data list -> 
  Types.symbol_table2 * (int * int)

val xdefine: 
  Types.symbol_table2 -> Types.symbol_table -> Types.symbol -> Types.section2 ->
  unit

