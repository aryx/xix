
val gen: 
  Exec_file.linker_config -> Exec_file.sections_size ->
  Types.word list (* code *) -> Types.byte array (* data *) ->
  Types.symbol_table2 (* for finding entry point *) ->
  Chan.o ->
  unit
