(* Copyright 2016 Yoann Padioleau, see copyright.txt *)
open Common

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type header = {
  magic: int;

  text_size: int;
  data_size: int;
  bss_size: int;

  symbol_size: int;
  pc_size: int;

  entry: int;
}

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* a.out uses big-endian integers even on low-endian architectures *)
(* TODO: rename blput ? big-end long put *)
let lput (chan : out_channel) (word : int) : unit =
  if word < 0 
  then raise (Impossible (spf "should call lput with a uint not %d" word));

  (* could also use land 0xFF ? what about negative numbers? *)
  let x1 = Char.chr (word mod 256) in
  let x2 = Char.chr ((word lsr 8) mod 256) in
  let x3 = Char.chr ((word lsr 16) mod 256) in
  let x4 = Char.chr ((word lsr 24) mod 256) in
  (* big part first; most-significant byte first *)
  output_char chan x4;
  output_char chan x3;
  output_char chan x2;
  output_char chan x1;
  ()

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

(* entry point *)
let write_header (header : header) (chan : out_channel) : unit =

  lput chan header.magic;

  lput chan header.text_size;
  lput chan header.data_size;
  lput chan header.bss_size;
  lput chan header.symbol_size;
  lput chan header.entry;
  lput chan 0;
  lput chan header.pc_size;
  ()
