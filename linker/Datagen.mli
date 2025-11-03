(*s: Datagen.mli *)

(*s: signature [[Datagen.gen]] *)
(* uses only sizes.data_size *)
val gen: 
  Types.symbol_table2 -> Types.addr (* init_data *) -> Exec_file.sections_size ->
  Types.data list ->
  Types.byte array
(*e: signature [[Datagen.gen]] *)
(*e: Datagen.mli *)
