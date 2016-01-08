
val gen: 
  Types.config -> Types.sections_size ->
  Types.word list (* code *) -> Types.word list (* data *) ->
  Types.symbol_table2 (* for finding entry point *) ->
  Common.filename ->
  unit

  
