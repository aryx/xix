
val gen: 
  Types.config -> Types.sections_size ->
  Types.word list (* code *) -> Types.byte array (* data *) ->
  Types.symbol_table2 (* for finding entry point *) ->
  Fpath.t ->
  unit

  
