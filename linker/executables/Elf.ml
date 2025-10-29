(* Copyright 2025 Yoann Padioleau, see copyright.txt *)
open Common

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* ELF executable format types and IO helpers *)

(*****************************************************************************)
(* Types and constants *)
(*****************************************************************************)

let exec_header_32_size = 52
let program_header_32_size = 32
let section_header_32_size = 40

(* ------------------------------------------------------------------------- *)
(* Exec header *)
(* ------------------------------------------------------------------------- *)

type exec_header = {
  ident: unit;
}

and ident_class =
  | CNone
  | C32
  | C64
  | CNum (* ?? *)

(* alt: ident_data *)
and byte_order = 
  | DNone
  | DLSB (* Least Significant Bit *)
  | DMSB (* Most Significant bit *)
  | DNum (* ?? *)

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
(* IO *)
(*****************************************************************************)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

(* entry point *)
let write_header
  (arch : Arch.t)
 (_sizes : Exec_file.sections_size) (_entry_addr : int) (_chan : out_channel) : unit =

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
  let endian = Arch.endian_of_arch arch in
  let _bo : byte_order =
    match endian  with
    | Endian.Little -> DLSB
    | Endian.Big -> DMSB
  in
  let _class_ : ident_class =
    match Arch.bits_of_arch arch with
    | Arch.Arch32 -> C32
    | Arch.Arch64 -> C64
  in
  
  failwith "XXX"
