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
[@@deriving show]

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

(* entry point *)
let write_header (header : header) (chan : out_channel) : unit =

  (* a.out uses big-endian integers even on low-endian architectures *)
  let lput = Endian.Big.output_32 in

  lput chan header.magic;

  lput chan header.text_size;
  lput chan header.data_size;
  lput chan header.bss_size;
  lput chan header.symbol_size;
  lput chan header.entry;
  lput chan 0;
  lput chan header.pc_size;
  ()
