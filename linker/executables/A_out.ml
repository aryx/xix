(*s: executables/A_out.ml *)
(* Copyright 2016 Yoann Padioleau, see copyright.txt *)
open Common

(*****************************************************************************)
(* Types *)
(*****************************************************************************)
(*s: constant [[A_out.header_size]] *)
let header_size = 32
(*e: constant [[A_out.header_size]] *)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)
(*s: function [[A_out.write_header]] *)
(* entry point *)
let write_header (arch : Arch.t) (sizes : Exec_file.sections_size) (entry_addr : int) (chan : out_channel) : unit =

  (* a.out uses big-endian integers even on low-endian architectures *)
  let output_32 = Endian.Big.output_32 in

  let magic =
    match arch with
    | Arch.Arm -> 0x647 (* Plan9 ARM magic *)
    | Arch.Mips -> failwith "TODO: A_out magic for Mips"
    | Arch.Riscv | Arch.Riscv64 | Arch.X86 | Arch.Amd64 | Arch.Arm64 ->
        failwith (spf "arch not supported yet: %s" (Arch.to_string arch))
  in

  output_32 chan magic; 
  output_32 chan sizes.text_size;
  output_32 chan sizes.data_size;
  output_32 chan sizes.bss_size;
  output_32 chan 0; (* TODO: symbol_size, for now stripped *)
  output_32 chan entry_addr;
  output_32 chan 0; (* ?? *)
  output_32 chan 0; (* pc_size *)
  ()
(*e: function [[A_out.write_header]] *)
(*e: executables/A_out.ml *)
