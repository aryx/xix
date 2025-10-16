
type header = {
  magic: int;

  text_size: int;
  data_size: int;
  bss_size: int;

  symbol_size: int;
  pc_size: int;

  entry: int;
}

val write_header: header -> out_channel -> unit
