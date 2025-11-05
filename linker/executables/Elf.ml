(*s: executables/Elf.ml *)
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

(*s: constant [[Elf.exec_header_32_size]] *)
let exec_header_32_size = 52
(*e: constant [[Elf.exec_header_32_size]] *)
(*s: constant [[Elf.program_header_32_size]] *)
let program_header_32_size = 32
(*e: constant [[Elf.program_header_32_size]] *)
(*s: constant [[Elf.section_header_32_size]] *)
let section_header_32_size = 40
(*e: constant [[Elf.section_header_32_size]] *)

(*s: constant [[Elf.nb_program_headers]] *)
(* Text, Data, and Symbol table *)
let nb_program_headers = 2 (* TODO 3 *)
(*e: constant [[Elf.nb_program_headers]] *)

(*s: constant [[Elf.header_size]] *)
let header_size =
  Int_.rnd (exec_header_32_size + nb_program_headers * program_header_32_size)
  16
(*e: constant [[Elf.header_size]] *)

(* ------------------------------------------------------------------------- *)
(* Exec header *)
(* ------------------------------------------------------------------------- *)

(*s: type [[Elf.ident_class]] *)
type ident_class =
  | CNone
  | C32
  | C64
  | CNum (* ?? *)
[@@warning "-37"]
(*e: type [[Elf.ident_class]] *)

(* alt: ident_data *)
(*s: type [[Elf.byte_order]] *)
type byte_order = 
  | BNone
  | BLSB (* Least Significant Bit *)
  | BMSB (* Most Significant bit *)
  | BNum (* ?? *)
[@@warning "-37"]
(*e: type [[Elf.byte_order]] *)

(*s: type [[Elf.ident_version]] *)
type ident_version =
  | VNone
  | VCurrent
[@@warning "-37"]
(*e: type [[Elf.ident_version]] *)

(*s: type [[Elf.elf_type]] *)
type elf_type =
  | TNone
  | TRel
  | TExec
  | TDyn
  | TCore
[@@warning "-37"]
(*e: type [[Elf.elf_type]] *)

(*s: type [[Elf.machine]] *)
type machine =
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
[@@warning "-37"]
(*e: type [[Elf.machine]] *)

(* ------------------------------------------------------------------------- *)
(* Program header *)
(* ------------------------------------------------------------------------- *)

(*s: type [[Elf.program_header_type]] *)
type program_header_type =
  | PH_None
  | PH_PT_Load
  | PH_Dynamic
  | PH_Interp
  | PH_Note
  | PH_Shlib
  | PH_Phdr
[@@warning "-37"]
(*e: type [[Elf.program_header_type]] *)

(*s: type [[Elf.program_header_protection]] *)
type program_header_protection =
  | R
  | W
  | X
(*e: type [[Elf.program_header_protection]] *)

(* ------------------------------------------------------------------------- *)
(* Section header *)
(* ------------------------------------------------------------------------- *)

(*****************************************************************************)
(* Conversions *)
(*****************************************************************************)
(*s: function [[Elf.byte_of_class]] *)
let byte_of_class (class_ : ident_class) : int =
  match class_ with
  | CNone -> 0
  | C32 -> 1
  | C64 -> 2
  | CNum -> 3
(*e: function [[Elf.byte_of_class]] *)

(*s: function [[Elf.byte_of_byte_order]] *)
let byte_of_byte_order (bo : byte_order) : int =
  match bo with
  | BNone -> 0
  | BLSB -> 1
  | BMSB -> 2
  | BNum -> 3
(*e: function [[Elf.byte_of_byte_order]] *)

(*s: function [[Elf.int_of_version]] *)
let int_of_version (v : ident_version) : int =
  match v with
  | VNone -> 0
  | VCurrent -> 1
(*e: function [[Elf.int_of_version]] *)

(*s: function [[Elf.int_of_elf_type]] *)
let int_of_elf_type (t : elf_type) : int =
  match t with
  | TNone -> 0
  | TRel -> 1
  | TExec -> 2
  | TDyn -> 3
  | TCore -> 4
(*e: function [[Elf.int_of_elf_type]] *)

(*s: function [[Elf.int_of_machine]] *)
let int_of_machine (m : machine) : int =
  match m with
  | MNone -> 0
  | MM32 -> 1
  | MSparc -> 2
  | MI386 -> 3
  | MM68K -> 4
  | MM88K -> 5
  | MI486 -> 6
  | MI860 -> 7
  | MMips -> 8
  | MS370 -> 9
  | MMipsr4K -> 10

  | MSparc64 -> 18
  | MPower -> 20
  | MPower64 -> 21

  | MArm -> 40
  | MAmd64 -> 62
  | MArm64 -> 183

  | MRiscv -> failwith "TODO: MRiscv"
(*e: function [[Elf.int_of_machine]] *)

(*s: function [[Elf.int_of_program_header_type]] *)
let int_of_program_header_type (ph : program_header_type) : int =
  match ph with
  | PH_None -> 0
  | PH_PT_Load -> 1
  | PH_Dynamic -> 2
  | PH_Interp -> 3
  | PH_Note -> 4
  | PH_Shlib -> 5
  | PH_Phdr -> 6
(*e: function [[Elf.int_of_program_header_type]] *)

(*s: function [[Elf.int_of_prot]] *)
let int_of_prot (prot : program_header_protection) : int =
  match prot with
  | R -> 0x4
  | W -> 0x2
  | X -> 0x1
(*e: function [[Elf.int_of_prot]] *)

(*s: function [[Elf.int_of_prots]] *)
let int_of_prots xs =
  List.fold_left (fun acc e -> acc + int_of_prot e) 0 xs
(*e: function [[Elf.int_of_prots]] *)

(*****************************************************************************)
(* IO *)
(*****************************************************************************)
(*s: function [[Elf.write_ident]] *)
(* first 16 bytes *)
let write_ident (bo : byte_order) (class_ : ident_class) (chan: out_channel) : unit =
  (* take care, using "\177ELF" like in C to OCaml does not work because
   * in C \177 is interpreted as an octal number but in OCaml it's an int
   * and 0o177 is different from 177. Simpler to use 0x7f.
   *)
  output_byte chan 0x7f;
  output_string chan "ELF";
  output_byte chan (byte_of_class class_);
  output_byte chan (byte_of_byte_order bo);
  output_byte chan (int_of_version VCurrent);
  output_byte chan 0; (* osabi = SYSV; 255 = boot/embedded/standalone? *)
  output_byte chan 0; (* abiversion = 3 *)
  output_string chan "\000\000\000\000\000\000\000";
  ()
(*e: function [[Elf.write_ident]] *)

(*s: function [[Elf.program_header_32]] *)
let program_header_32 (endian: Endian.t) (ph: program_header_type)
  offset (vaddr, paddr) (filesz, memsz) prots align (chan : out_channel) =
  let (_, output_32) = Endian.output_functions_of_endian endian in
  output_32 chan (int_of_program_header_type ph);
  output_32 chan offset;
  output_32 chan vaddr;
  output_32 chan paddr;
  output_32 chan filesz;
  output_32 chan memsz;
  output_32 chan (int_of_prots prots);
  output_32 chan align
(*e: function [[Elf.program_header_32]] *)
  
(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)
(*s: function [[Elf.write_headers]] *)
(* entry point *)
let write_headers (config : Exec_file.linker_config)
 (sizes : Exec_file.sections_size) (entry_addr : int) (chan : out_channel) : int * int =
  let arch = config.arch in

  (* ELF ident part (first 16 bytes) *)

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

  (* Rest of ELF header (36 bytes => total 52 bytes) *)

  let mach : machine =
    match arch with
    | Arch.Arm -> MArm
    | Arch.Arm64 -> MArm64
    | Arch.Mips -> MMips
    | Arch.Riscv -> MRiscv
    | Arch.Riscv64 -> failwith "TODO: Riscv64"
    | Arch.X86 -> MI386 (* what about MI486? *)
    | Arch.Amd64 -> MAmd64
  in
  let output_16, output_32 = Endian.output_functions_of_endian endian in
  output_16 chan (int_of_elf_type TExec);
  output_16 chan (int_of_machine mach);
  output_32 chan (int_of_version VCurrent); (* again? *)
  output_32 chan entry_addr;
  output_32 chan exec_header_32_size; (* offset to first phdr *)
  output_32 chan 0; (* TODO: offset to first shdr HEADR+textsize+datsize+symsize *)
  (match arch with
  | Arch.Arm ->
        (* version5 EABI for Linux *)
        output_32 chan 0x5000200;
  | _ -> output_32 chan 0
  );
  output_16 chan exec_header_32_size;
  output_16 chan program_header_32_size;
  output_16 chan nb_program_headers; (* # of Phdrs *)
  output_16 chan section_header_32_size;
  output_16 chan 0; (* # of Shdrs, TODO 3 *)
  output_16 chan 0; (* Shdr table index, TODO 2 *)

  Logs.debug (fun m -> m "after ELF header at pos %d" (pos_out chan));

  (* Program headers *)

  (* Text *)
  let offset_disk_text = config.header_size in
  program_header_32 endian PH_PT_Load 
   offset_disk_text (config.init_text, config.init_text)
    (sizes.text_size, sizes.text_size) [R; X] config.init_round chan;

  (* ELF Linux constrains that virtual
   * address modulo a page must match file offset modulo
   * a page, so simpler to start data at a page boundary
   *)
  let offset_disk_data = 
    Int_.rnd (config.header_size + sizes.text_size) config.init_round
  in
  let init_data =
    match config.init_data with
    | None -> 
        raise (Impossible "init_data should be set by now after layout_text")
    | Some x -> x
  in
  program_header_32 endian PH_PT_Load
    offset_disk_data (init_data, init_data)
     (sizes.data_size, sizes.data_size + sizes.bss_size) [R; W; X]
     config.init_round chan;

  Logs.debug (fun m -> m "after Program headers at pos %d" (pos_out chan));
  offset_disk_text, offset_disk_data
(*e: function [[Elf.write_headers]] *)
(*e: executables/Elf.ml *)
