(* Copyright 2016 Yoann Padioleau, see copyright.txt *)
open Common

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

let header_size = 32

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
let write_header (sizes : Exec_file.sections_size) (entry_addr : int) (chan : out_channel) : unit =

  let header = {
      (* Plan 9 ARM *)
      magic = 0x647;

      text_size = sizes.Exec_file.text_size;
      data_size = sizes.Exec_file.data_size;
      bss_size = sizes.Exec_file.bss_size;

      (* todo: for now stripped *)
      symbol_size = 0;
      pc_size = 0;
     
      entry = entry_addr;
    }
  in

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
