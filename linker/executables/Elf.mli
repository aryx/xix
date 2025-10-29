
val header_size: int

val write_headers: 
  Exec_file.linker_config -> Exec_file.sections_size -> int (* entry_addr *) ->
  out_channel -> unit