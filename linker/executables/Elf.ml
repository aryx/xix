(* Copyright 2025 Yoann Padioleau, see copyright.txt *)
open Common

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* ELF executable format types and IO helpers.
 *
 * spec: ???
*)

(*****************************************************************************)
(* Types and constants *)
(*****************************************************************************)

let exec_header_32_size = 52
let program_header_32_size = 32
let section_header_32_size = 40

(* ------------------------------------------------------------------------- *)
(* Exec header *)
(* ------------------------------------------------------------------------- *)

type ident_class =
  | CNone
  | C32
  | C64
  | CNum (* ?? *)

(* alt: ident_data *)
and byte_order = 
  | BNone
  | BLSB (* Least Significant Bit *)
  | BMSB (* Most Significant bit *)
  | BNum (* ?? *)

and machine =
  | MNone

  (* main one; the one we want to support in xix *)
  | MI386
  | MAmd64
  | MArm
  | MArm64
  | MMips
  | MRiscv
  (* | MRiscv64? *)

  (* other: *)
  | MM32
  | MSparc
  | MM68K
  | MM88K
  | MI486
  | MI860
  | MS370
  | MMipsr4K
  | MSparc64
  | MPower
  | MPower64

(* ------------------------------------------------------------------------- *)
(* Program header *)
(* ------------------------------------------------------------------------- *)

type program_header_type =
  | PH_None
  | PH_PT_Load
  | PH_Dynamic
  | PH_Interp
  | PH_Note
  | PH_Shlib
  | PH_Phdr

(* ------------------------------------------------------------------------- *)
(* Section header *)
(* ------------------------------------------------------------------------- *)

(*****************************************************************************)
(* Conversions *)
(*****************************************************************************)
let byte_of_class (class_ : ident_class) : int =
  match class_ with
  | CNone -> 0
  | C32 -> 1
  | C64 -> 2
  | CNum -> 3

let byte_of_byte_order (bo : byte_order) : int =
  match bo with
  | BNone -> 0
  | BLSB -> 1
  | BMSB -> 2
  | BNum -> 3

(*****************************************************************************)
(* IO *)
(*****************************************************************************)

(* first 16 bytes *)
let write_ident (bo : byte_order) (class_ : ident_class) (chan: out_channel) : unit =
  output_string chan "\177ELF";
  output_byte chan (byte_of_class class_);
  output_byte chan (byte_of_byte_order bo);
  output_byte chan 1; (* version = CURRENT *)
  output_byte chan 0; (* osabi = SYSV; 255 = boot/embedded/standalone? *)
  output_byte chan 0; (* abiversion = 3 *)
  output_string chan "\000\000\000\000\000\000\000";
  ()
  
(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

(* entry point *)
let write_header (arch : Arch.t)
 (_sizes : Exec_file.sections_size) (_entry_addr : int) (chan : out_channel) : unit =
  let endian = Arch.endian_of_arch arch in
  let bo : byte_order =
    match endian  with
    | Endian.Little -> BLSB
    | Endian.Big -> BMSB
  in
  let class_ : ident_class =
    match Arch.bits_of_arch arch with
    | Arch.Arch32 -> C32
    | Arch.Arch64 -> C64
  in
  write_ident bo class_ chan;

  let _mach : machine =
    match arch with
    | Arch.Arm -> MArm
    | Arch.Arm64 -> MArm64
    | Arch.Mips -> MMips
    | Arch.Riscv -> MRiscv
    | Arch.Riscv64 -> failwith "TODO: Riscv64"
    | Arch.X86 -> MI386 (* what about MI486? *)
    | Arch.Amd64 -> MAmd64
  in
  let _output_16, _output_32 = Endian.output_functions_of_endian endian in

  failwith "XXX1"
