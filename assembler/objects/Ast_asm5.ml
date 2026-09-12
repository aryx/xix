(*s: objects/Ast_asm5.ml *)
(* Copyright 2015, 2016 Yoann Padioleau, see copyright.txt *)
open Common

module A = Ast_asm
open Ast_asm

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Abstract Syntax Tree (AST) for the assembly language supported by 5a.
 * I call this language Asm5.
 *
 * Note that many types are now defined in Ast_asm.ml instead because they are
 * mostly arch independent and can be reused in other plan9 assemblers
 * (e.g. va, ia, 7a).
 *
 * Asm5 is a simple RISC assembly language. The main innovations of ARM
 * are the shifted registers operands, to easily represent
 * big and "useful" integers (integers which are powers of 2) in a compact way,
 * and the fact that every instruction can be tagged with a condition.
 * Like in the MIPS there is a LINK register.
 *
 * !!! If you modify this file please increment Object_file.version !!!
 * 
 * TODO:
 *  - 5c-only opcodes? CASE, BCASE, MULU/DIVU/MODU (or better in Ast_asm.ml too?)
 *  - handle the instructions used in the kernel
 * (claude: MULA/MULL, MOVM (and its .IA/.DB/etc special bits), PSR,
 * and MCR/MRC are now implemented -- see MULL, MOVM/movm_addr_mode,
 * PSRImsr/psrreg, and Parser_asm5.mly's MCR/MRC pseudo_instr
 * production below/there.)
 *)

(*****************************************************************************)
(* The AST related types *)
(*****************************************************************************)

(* ------------------------------------------------------------------------- *)
(* Numbers and Strings *)
(* ------------------------------------------------------------------------- *)
(* see Ast_asm.ml *)

(* ------------------------------------------------------------------------- *)
(* Operands *)
(* ------------------------------------------------------------------------- *)
(*s: type [[Ast_asm5.reg]] *)
type reg = A.register (* between 0 and 15 *)
(*e: type [[Ast_asm5.reg]] *)
[@@deriving show]

(*s: type [[Ast_asm5.freg]] *)
type freg = A.fregister (* between 0 and 15 *)
(*e: type [[Ast_asm5.freg]] *)
[@@deriving show]

(* ?? *)
(*s: type [[Ast_asm5.creg]] *)
type creg = C of int (* between 0 and 15 *)
(*e: type [[Ast_asm5.creg]] *)

(* claude: case 56/57 (move to/from FP[CS]R) -- FPA's status/control
 * registers, goken's D_FPCR ("FPSR"/"FPCR" tokens, lex.c: FPSR=0,
 * FPCR=1). *)
type fcrreg = FPSR | FPCR
[@@deriving show]

(* claude: case 35/36/37 (mov PSR,R / mov R,PSR / mov $con,PSR) --
 * goken's D_PSR ("CPSR"/"SPSR" tokens, lex.c: CPSR=0, SPSR=1). The
 * ".F" (flags-only write) suffix isn't wired: the generic dot-
 * suffix-flag mechanism (`condf`/TSUF, see movm_addr_mode below)
 * exists for MOVM's P/U/W bits, but no PSR production uses it, so
 * ".F" stays unparseable here. The default (unset, "full PSR write")
 * is still a real, useful, independently-testable instruction shape
 * on its own. *)
type psrreg = CPSR | SPSR
[@@deriving show]

(* claude: case 38/39 (movm $con,oreg -> stm / movm oreg,$con -> ldm,
 * ARM's block data transfer, i.e. multi-register load/store). Unlike
 * PSR's lone ".F" bit above, MOVM's P/U/W address-mode suffix bits
 * (goken's C_PBIT/C_UBIT/C_WBIT, lex.c's ".IA"/".DB"/".IAW"/".DBW"/
 * etc tokens) are essential to any real use -- e.g. a function
 * prologue push is written "MOVM.DB.W [regs],(SP)" -- so this is what
 * finally justified building a real generic dot-suffix-flag grammar
 * mechanism (see Parser_asm5.mly's `condf` rule and TSUF token, a
 * left-recursive bitmask accumulator directly mirroring goken's own
 * `cond: cond LS { $1 | $2 }`). `movm_addr_mode` is that mechanism's
 * decoded payload for MOVM specifically -- a plain P/U/W bitset.
 * The S bit (C_SBIT, PSR transfer on the LDM/STM used by exception
 * return) is deliberately not wired: RFE already emits its own fixed
 * MOVM encoding by hand (see Codegen5.ml's RFE case), so no real .s
 * text needs to spell MOVM.S directly -- Parser_asm5.mly's MOVM
 * productions reject it with a real error instead of silently
 * ignoring it (same "error loudly, don't emit wrong bytes" precedent
 * as case 37's immrot check). *)
type movm_addr_mode = {
  mm_pre : bool;       (* P: pre-index vs post-index *)
  mm_up : bool;        (* U: increment vs decrement (address direction) *)
  mm_writeback : bool; (* W: write address back to the base register *)
}
[@@deriving show]

(* claude: bit values for the generic dot-suffix-flag token (TSUF),
 * one per suffix xix's lexer recognizes, so that multiple suffixes
 * fold via plain bitwise-or in Parser_asm5.mly's `condf` rule -- the
 * same accumulator shape as goken's `scond`. Only sflag_pbit/
 * sflag_ubit/sflag_wbit are actually decoded anywhere
 * (movm_addr_mode_of_flags below); sflag_sbit/sflag_fbit exist so the
 * full suffix vocabulary can be recognized (and explicitly rejected
 * where unsupported, e.g. MOVM.S) instead of being a lexer/grammar
 * error that looks unrelated to the real reason.
 * NOTE: unlike goken's own lex.c, these are 5 independent bits.
 * goken actually reuses ONE physical bit (C_UBIT and C_FBIT are both
 * `1<<7`, GO/C/cmd/5l/5.out.h) for two unrelated meanings ("up" on
 * MOVM, "flags-only" on MSR) -- a bit-packing accident, not a real
 * language feature (nobody writes "MOVM.F" expecting up-bit
 * behavior). Confirmed directly: assembling+linking
 * "MOVM.F.DB.W [R4,R5],(R13)" with goken produces the exact same
 * bytes as "MOVM.U.DB.W" would. Replicating that aliasing quirk
 * bit-for-bit isn't worth it for something no real .s text relies
 * on -- xix instead keeps ".F" and ".U" as textually and bit-wise
 * distinct tokens, so MOVM.F is simply rejected (see the MOVM
 * productions in Parser_asm5.mly) rather than silently behaving like
 * MOVM.U. *)
let sflag_sbit = 1
let sflag_pbit = 2
let sflag_ubit = 4
let sflag_wbit = 8
let sflag_fbit = 16

let movm_addr_mode_of_flags (flags : int) : movm_addr_mode =
  { mm_pre = flags land sflag_pbit <> 0;
    mm_up = flags land sflag_ubit <> 0;
    mm_writeback = flags land sflag_wbit <> 0;
  }

(* reserved by linker *)
(*s: constant [[Ast_asm5.rTMP]] *)
let rTMP = R 11
(*e: constant [[Ast_asm5.rTMP]] *)
(*s: constant [[Ast_asm5.rSB]] *)
let rSB  = R 12
(*e: constant [[Ast_asm5.rSB]] *)
(*s: constant [[Ast_asm5.rSP]] *)
let rSP  = R 13
(*e: constant [[Ast_asm5.rSP]] *)
(* reserved by hardware *)
(*s: constant [[Ast_asm5.rLINK]] *)
let rLINK = R 14
(*e: constant [[Ast_asm5.rLINK]] *)
(*s: constant [[Ast_asm5.rPC]] *)
let rPC   = R 15
(*e: constant [[Ast_asm5.rPC]] *)

(* used by the compiler for calling conventions *)
let rRET = R 0

(*s: constant [[Ast_asm5.nb_registers]] *)
let nb_registers = 16
(*e: constant [[Ast_asm5.nb_registers]] *)
let nb_fregisters = 16

(*s: type [[Ast_asm5.arith_operand]] *)
type arith_operand =
  | Imm of A.integer (* characters are converted to integers *)
  | Reg of reg
  (* can not be used with shift opcodes (SLL/SRL/SRA) *)
  | Shift of reg * shift_reg_op * 
             (reg, int (* between 0 and 31 *)) Either_.t
(*e: type [[Ast_asm5.arith_operand]] *)

(*s: type [[Ast_asm5.shift_reg_op]] *)
  and shift_reg_op =
    | Sh_logic_left | Sh_logic_right
    | Sh_arith_right | Sh_rotate_right
(*e: type [[Ast_asm5.shift_reg_op]] *)
[@@deriving show]

(* alt: could almost be moved to Ast_asm.ml but Shift above of arith_operand
 * is arm-specific
 *)
(*s: type [[Ast_asm5.mov_operand]] *)
type mov_operand = 
  (* Immediate shift register *)
  | Imsr of arith_operand
  (* eXtended immediate.
   * (Ximm (Int x) is converted in Imsr (Imm x) in the parser
   *)
  | Ximm of A.ximm

  | Indirect of reg * A.offset
  (* another form of Indirect *)
  | Entity of A.entity
  (* claude: the register side of a float load/store (case 50/51/52/
   * 53, MOVEF below) -- the memory side still uses Indirect/Entity
   * above unchanged, since FPA/VFP load/store still addresses memory
   * via a plain *integer* base register + offset, only the data
   * register being loaded/stored is a float one. *)
  | FImsr of freg
  (* claude: case 56/57 -- goken dispatches these through the exact
   * same "MOVW" mnemonic/gen mechanism as ordinary int moves (see
   * a.y's `gen: ... | LFCR | LPSR`), not a distinct mnemonic like
   * MOVF/MOVD, so this is a mov_operand alternative (used with the
   * existing MOVE instr) rather than a new instr constructor. *)
  | FCRImsr of fcrreg
  (* claude: case 35/36/37 -- same "MOVW"/gen mechanism as FCRImsr
   * above, see psrreg's own comment for why the ".F" suffix isn't
   * wired. *)
  | PSRImsr of psrreg
  (* claude: case 38/39 -- the "[R4-R11,R14]" register-list operand of
   * MOVM below, a plain bitmask (bit i set means Ri is in the list),
   * mirroring goken's own D_CONST reglist-bitmask representation
   * (a.y's `reglist` rule folds ranges/commas into one int the same
   * way). Paired with an Indirect base register via MOVM; direction
   * (store vs load) is inferred from which side is RegList vs
   * Indirect, the same convention MOVE already uses for its src/dst. *)
  | RegList of int
(*e: type [[Ast_asm5.mov_operand]] *)

[@@deriving show]

(* ------------------------------------------------------------------------- *)
(* Instructions *)
(* ------------------------------------------------------------------------- *)

(* less: could probably factorize things and move stuff in Ast_asm.ml *)
(*s: type [[Ast_asm5.instr]] *)
type instr = 
  (* Arithmetic *)
  (*s: [[Ast_asm5.instr]] arithmetic instructions cases *)
  | Arith of arith_opcode * arith_cond option *
      arith_operand (* src *) * reg option * reg (* dst *)
  (*x: [[Ast_asm5.instr]] arithmetic instructions cases *)
  | ArithF of (arithf_opcode * A.floatp_precision) *
      (A.floatp, freg) Either_.t * freg option * freg
  (* claude: case 55 (FPA)/76 (VFP): fix and float, i.e. int<->float
   * conversion (goken's AMOVWF/AMOVFW/AMOVWD/AMOVDW). Direction is
   * baked into which constructor is used, matching the mnemonic,
   * rather than a shared from/to operand-order convention like
   * ArithF/MOVE. *)
  | MOVWF of A.floatp_precision * reg * freg (* int -> float/double *)
  | MOVFW of A.floatp_precision * freg * reg (* float/double -> int *)
  (* claude: case 17 -- 64-bit long multiply, register-pair result.
   * The 4 mnemonics (MULL/MULLU/MULAL/MULALU) are just
   * sign x accumulate-or-not, goken's own oprrr() encodes them as
   * such (a 2-bit sub-field), so one constructor covers all 4
   * instead of a small enum type -- "MULL r1,r2,(hi,lo)" syntax
   * (goken's a.y `regreg: '(' regi ',' regi ')'`). *)
  | MULL of A.sign * bool (* accumulate *) *
      reg (* r1, from *) * reg (* r2, middle *) *
      reg (* hi *) * reg (* lo *)
  (*e: [[Ast_asm5.instr]] arithmetic instructions cases *)

  (* Memory *)
  (*s: [[Ast_asm5.instr]] memory instructions cases *)
  | MOVE of A.move_size * move_option *
      mov_operand (* src *) * mov_operand (* dst *) (* virtual *)
  | SWAP of A.move_size (* actually only (Byte x) *) *
       reg (* indirect *) * reg * reg option
  (* claude: case 50/51/52/53 (MOVF/MOVD load/store) -- kept as its
   * own constructor rather than folded into MOVE, since MOVE's size
   * is Ast_asm.move_size (Word/Byte/HalfWord, shared across archs)
   * with no Float case, and adding one there would be a cross-arch
   * change for an ARM-only feature -- floatp_precision (F/D) already
   * exists and is exactly what's needed instead. *)
  | MOVEF of A.floatp_precision *
      mov_operand (* src *) * mov_operand (* dst *)
  (* claude: case 38 (movm $con,oreg -> stm, "MOVM [regs],(Rbase)")
   * and case 39 (movm oreg,$con -> ldm, "MOVM (Rbase),[regs]") -- see
   * movm_addr_mode's own comment for the P/U/W suffix bits and the
   * S-bit caveat. *)
  | MOVM of movm_addr_mode *
      mov_operand (* src *) * mov_operand (* dst *)
  (*e: [[Ast_asm5.instr]] memory instructions cases *)

  (* Control flow *)
  (*s: [[Ast_asm5.instr]] control-flow instructions cases *)
  | B  of A.branch_operand (* branch *)
  | BL of A.branch_operand (* branch and link *)
  | Cmp of cmp_opcode * arith_operand * reg
  (* just Relative or LabelUse here for branch_operand *)
  | Bxx of condition * A.branch_operand (* virtual, sugar for B.XX *) 
  (*x: [[Ast_asm5.instr]] control-flow instructions cases *)
  | CmpF of A.floatp_precision * freg * freg
  (*e: [[Ast_asm5.instr]] control-flow instructions cases *)

  (* System *)
  (*s: [[Ast_asm5.instr]] system instructions cases *)
  | SWI of int (* value actually unused in Plan 9 and Linux *)
  | RFE (* virtual, sugar for MOVM *)
  (*e: [[Ast_asm5.instr]] system instructions cases *)
(*e: type [[Ast_asm5.instr]] *)

(*s: type [[Ast_asm5.arith_opcode]] *)
  and arith_opcode = 
    (* logic *)
    | AND | ORR | EOR
    (* arithmetic *)
    | ADD | SUB   | MUL   | DIV | MOD (* DIV and MOD are virtual *)
    (* bit shifting; immediate operand can only be between 0 and 31 *)
    | SLL | SRL | SRA (* virtual, sugar for bitshift register *)
    (* less useful *)
    | BIC  | ADC | SBC  | RSB | RSC
    (* middle operand always empty (could lift up and put special type) *)
    | MOV | MVN (* MOV has no reading syntax in 5a, MOVE is used *)
(*e: type [[Ast_asm5.arith_opcode]] *)
(*s: type [[Ast_asm5.arith_cond]] *)
  and arith_cond = Set_condition (* .S *)
(*e: type [[Ast_asm5.arith_cond]] *)

(*s: type [[Ast_asm5.arithf_opcode]] *)
  and arithf_opcode =
    | ADD_ | SUB_ | MUL_ | DIV_
(*e: type [[Ast_asm5.arithf_opcode]] *)
    
(*s: type [[Ast_asm5.cmp_opcode]] *)
  and cmp_opcode = 
    | CMP
    (* less useful *)
    | TST | TEQ | CMN
(*e: type [[Ast_asm5.cmp_opcode]] *)

(*s: type [[Ast_asm5.condition]] *)
  and condition =
    (* equal, not equal *)
    | EQ | NE
    (* greater than, less than, greater or equal, less or equal *)
    | GT of sign | LT of sign | GE of sign | LE of sign
    (* minus/negative, plus/positive *)
    | MI | PL 
    (* overflow set/clear *)
    | VS | VC
    (* always/never *)
    | AL | NV
(*e: type [[Ast_asm5.condition]] *)

(*s: type [[Ast_asm5.move_option]] *)
   and move_option = move_cond option
(*e: type [[Ast_asm5.move_option]] *)
     (* this is used only with a MOV with an indirect with offset operand *)
(*s: type [[Ast_asm5.move_cond]] *)
     and move_cond = WriteAddressBase (* .W *) | PostOffsetWrite (* .P *)
(*e: type [[Ast_asm5.move_cond]] *)

[@@deriving show]

(* claude: the raw 4-bit ARM condition-code value, e.g. for MCR/MRC's
 * grammar action (Parser_asm5.mly) which builds its final encoded
 * word directly at parse time, bypassing Codegen5.ml entirely --
 * unlike Codegen5.ml's own `gcond` (same mapping, but returns a
 * Bits.t tuple, and lives in a different dune library the assembler
 * doesn't depend on). *)
let int_of_condition (c : condition) : int =
  match c with
  | EQ     -> 0x0
  | NE     -> 0x1
  | GE (U) -> 0x2
  | LT (U) -> 0x3
  | MI     -> 0x4
  | PL     -> 0x5
  | VS     -> 0x6
  | VC     -> 0x7
  | GT (U) -> 0x8
  | LE (U) -> 0x9
  | GE (S) -> 0xa
  | LT (S) -> 0xb
  | GT (S) -> 0xc
  | LE (S) -> 0xd
  | AL     -> 0xe
  | NV     -> 0xf

(* ------------------------------------------------------------------------- *)
(* Program *)
(* ------------------------------------------------------------------------- *)

(*s: type [[Ast_asm5.instr_with_cond]] *)
type instr_with_cond = instr * condition
(*e: type [[Ast_asm5.instr_with_cond]] *)
[@@deriving show]

(*s: type [[Ast_asm5.program]] *)
(* On the ARM every instructions can be prefixed with a condition.
 * Note that cond should be AL (Always) for B/Bxx instructions.
*)
type program = instr_with_cond A.program
(*e: type [[Ast_asm5.program]] *)
[@@deriving show]

(*****************************************************************************)
(* Extractors/Visitors *)
(*****************************************************************************)

(*s: function [[Ast_asm5.branch_opd_of_instr]] *)
let branch_opd_of_instr (instr : instr_with_cond) : A.branch_operand option =
  (* less: could issue warning if cond <> AL when B or Bxx, or normalize? *)
  match fst instr with
  (* ocaml-light: | B opd | BL opd | Bxx (_, opd) -> *)
  | B opd -> Some opd
  | BL opd -> Some opd
  | Bxx (_cond, opd) -> Some opd
  | Arith _ | ArithF _ | MOVWF _ | MOVFW _ | MOVE _ | MOVEF _ | SWAP _
  | Cmp _ | CmpF _ | SWI _ | RFE | MULL _ | MOVM _ -> None
(*e: function [[Ast_asm5.branch_opd_of_instr]] *)

(*s: function [[Ast_asm5.visit_globals_instr]] *)
let visit_globals_instr (f : global -> unit) (i : instr_with_cond) : unit =
  let mov_operand x =
    match x with
    | Entity (A.Global (x, _)) -> f x
    | Entity (A.Param _ | A.Local _) -> ()
    | Ximm x -> A.visit_globals_ximm f x
    | Imsr _ | Indirect _ | FImsr _ | FCRImsr _ | PSRImsr _ | RegList _ -> ()
  in
  match fst i with
  | MOVE (_, _, m1, m2) -> mov_operand m1; mov_operand m2
  | MOVEF (_, m1, m2) -> mov_operand m1; mov_operand m2
  | MOVM (_, m1, m2) -> mov_operand m1; mov_operand m2
  (* ocaml-light: | B b | BL b | Bxx (_, b) -> branch_operand b *)
  | B b -> A.visit_globals_branch_operand f b
  | BL b -> A.visit_globals_branch_operand f b
  | Bxx (_, b) -> A.visit_globals_branch_operand f b
  | Arith _ | ArithF _ | MOVWF _ | MOVFW _ | SWAP _ | Cmp _ | CmpF _ | SWI _
  | RFE | MULL _ -> ()
(*e: function [[Ast_asm5.visit_globals_instr]] *)
(*e: objects/Ast_asm5.ml *)
