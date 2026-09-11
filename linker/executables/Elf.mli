(*s: executables/Elf.mli *)

(*s: signature [[Elf.header_size]] *)
(* claude: parameterized by bits (was a plain constant) since ELF64
 * (riscv64/ojl) needs a different header_size than ELF32 *)
val header_size: Arch.bits -> int
(*e: signature [[Elf.header_size]] *)

(*s: signature [[Elf.write_headers]] *)
(* return offset_disk_text and offset_disk_data for the caller to use seek_out *)
val write_headers: 
  Exec_file.linker_config -> Exec_file.sections_size -> int (* entry_addr *) ->
  out_channel -> int * int
(*e: signature [[Elf.write_headers]] *)

(* claude: section header table; seeks the channel itself, see Elf.ml *)
val write_sections:
  Exec_file.linker_config -> Exec_file.sections_size -> out_channel -> unit
(*e: executables/Elf.mli *)
