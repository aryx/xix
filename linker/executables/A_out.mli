(*s: executables/A_out.mli *)

(*s: signature [[A_out.header_size]] *)
val header_size: int
(*e: signature [[A_out.header_size]] *)

(*s: signature [[A_out.write_header]] *)
val write_header: 
  Arch.t ->
  Exec_file.sections_size -> int (* entry_addr *) -> out_channel -> unit
(*e: signature [[A_out.write_header]] *)
(*e: executables/A_out.mli *)
