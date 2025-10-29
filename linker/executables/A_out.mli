
type header = {
  magic: int;

  text_size: int;
  data_size: int;
  bss_size: int;

  symbol_size: int;
  pc_size: int;

  entry: int;
}
[@@deriving show]

val header_size: int

val write_header: 
  Exec_file.sections_size -> int (* entry_addr *) -> out_channel -> unit
