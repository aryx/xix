
val header_size: int

(* return offset_disk_text and offset_disk_data for the caller to use seek_out *)
val write_headers: 
  Exec_file.linker_config -> Exec_file.sections_size -> int (* entry_addr *) ->
  out_channel -> int * int