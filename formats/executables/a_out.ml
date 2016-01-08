open Common

type header = {
  magic: int;
  
  text_size: int;
  data_size: int;
  bss_size: int;
  symbol_size: int;
  pc_size: int;

  entry: int;
}

let write_header header chan =
  if true then raise Todo
