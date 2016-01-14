open Common

type header = {
  text_size: int;
  data_size: int;
  bss_size: int;

  symbol_size: int;
  pc_size: int;

  entry: int;
}

let lput chan word =
  if word < 0 
  then raise (Impossible (spf "should call lput with uint not %d" word));

  let x1 = Char.chr (word mod 256) in
  let x2 = Char.chr ((word lsr 8) mod 256) in
  let x3 = Char.chr ((word lsr 16) mod 256) in
  let x4 = Char.chr ((word lsr 24) mod 256) in
  output_char chan x4;
  output_char chan x3;
  output_char chan x2;
  output_char chan x1;
  ()

let write_header header chan =
  lput chan 0x647;
  lput chan header.text_size;
  lput chan header.data_size;
  lput chan header.bss_size;
  lput chan header.symbol_size;
  lput chan header.entry;
  lput chan 0;
  lput chan header.pc_size;
  ()
  
