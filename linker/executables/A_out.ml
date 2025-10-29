(* Copyright 2016 Yoann Padioleau, see copyright.txt *)
open Common

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

let header_size = 32

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

(* entry point *)
let write_header (sizes : Exec_file.sections_size) (entry_addr : int) (chan : out_channel) : unit =

  (* a.out uses big-endian integers even on low-endian architectures *)
  let output_32 = Endian.Big.output_32 in

  output_32 chan 0x647; (* Plan9 ARM magic *)
  output_32 chan sizes.text_size;
  output_32 chan sizes.data_size;
  output_32 chan sizes.bss_size;
  output_32 chan 0; (* TODO: symbol_size, for now stripped *)
  output_32 chan entry_addr;
  output_32 chan 0; (* ?? *)
  output_32 chan 0; (* pc_size *)
  ()
