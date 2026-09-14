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
 *  - handle the instructions used in the kernel
 * (claude: CASE/BCASE and MULU/DIVU/MODU, this comment's other old
 * TODO item, are now implemented -- see CASE/BCASE below and
 * arith_opcode's DIVU/MODU. Confirmed against goken's real 5a lexer
 * table: DIV/MOD themselves ARE real 5a mnemonics (real ARM has no
 * hardware divide, so goken's own linker expands them into a
 * software-helper call sequence -- see linkers/5l/noop.c's ADIV/
 * AMOD/ADIVU/AMODU case), but MULU/DIVU/MODU (the *unsigned*
 * variants) are NOT -- 5c-internal-only opcode values that happen to
 * print under those names, same category as CASE/BCASE and .CC/.CS
 * elsewhere in this port. This port's own DIVU/MODU additionally
 * deviate from goken's real *encoding* too, not just parseability:
 * real hardware SDIV/UDIV (Codegen5.ml's own comment) instead of
 * goken's software-call-to-arch/arm/div.s expansion, since goken's
 * expansion needs div.s's own "NAME = value" + "R(name)" constant-
 * register-alias syntax, a separate, larger, not-yet-attempted
 * feature -- a deliberate, documented substitution for this
 * stress-testing pipeline (functional correctness under qemu, not
 * goken byte/behavior parity, is what matters here), not a claim
 * that o5l now replicates goken's real division sequence. See
 * docs/claude_notes/plan_hello_libc_linking.md.)
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
  (* claude: a scaled-register-offset memory address, e.g. real 5c -S
   * output for fmt/dofmt.c's "MOVB R7<<0(R3),R3" (base R3, index R7,
   * shift amount 0). NOT real 5a syntax either (confirmed: goken's
   * own real 5a rejects it -- there is no source-level way to write
   * a register-offset memory address in real 5a at all; Codegen5.ml's
   * gmem already has an Either.Right register-offset encoding path,
   * but it was purely a linker-internal fallback for an
   * immediate-offset-too-large case, e.g. case 30/31's REGTMP
   * addressing, never reachable from parsed text before this). A
   * xix-only pipeline accommodation, same category as CASE/BCASE,
   * .CC/.CS, and BL's "0(Rn)" elsewhere in this port -- see
   * docs/claude_notes/plan_hello_libc_linking.md. Only LSL is
   * verified (every real -S occurrence found so far uses it -- shift
   * amount 0 for byte-array indexing, e.g. dofmt.c, or a real scale
   * like 2 for word-array indexing, e.g. fltfmt.c); Codegen5.ml
   * raises Todo for any other shift type, and for Byte S/HalfWord
   * (ghalfword's register-offset addressing mode has no shift field
   * on real ARM hardware at all, unlike plain LDR/STR's gmem) rather
   * than emit unverified bytes. *)
  | IndirectShift of reg (* index *) * shift_reg_op *
      (reg, int) Either_.t (* shift amount *) * reg (* base *)
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
  (* claude: switch-statement jump-table dispatch, e.g. real 5c -S
   * output for a dense-range switch: "CMP $range,Rn; CASE.LS Rn;
   * BHI default; BCASE case0; BCASE case1; ...". Ported from goken's
   * real 5c (compilers/5c/swt.c's swit2, "direct:" label) and 5l
   * (linkers/5l/codegen.c's case 62/63): CASE Rn (real final encoding
   * "LDR{cond} PC,[PC,Rn,LSL#2]", goken's own comment: "movw
   * R<<2(PC),PC") is the indexed jump into the table that immediately
   * follows it in the instruction stream; each BCASE entry is NOT a
   * real instruction at all, just a raw data word holding its
   * target's final resolved address (goken's codegen.c case 63:
   * "o1 = p->cond->pc", no encoding, see Codegen5.ml). Unlike every
   * other instr here, this pair has NO real 5a grammar to match at
   * all -- 5c never round-trips switch-statement code through the
   * assembler's text parser, it builds these Prog structures directly,
   * and (confirmed directly against assemblers/5a/a.y/lex.c) 5a's own
   * grammar has no CASE/BCASE production either -- so this concrete
   * syntax (and the choice to give BCASE no condition, since a table
   * entry isn't a predicated instruction) is a xix-only extension for
   * this pipeline on ARM32, not real-5a parity. (ARM64's own 7a is
   * different: assemblers/7a/lex.c/a.y genuinely DO lex and parse
   * "CASE"/"BCASE" as real ACASE/ABCASE tokens with real productions
   * -- see Ast_asm7.ml's CaseJump/BCase, which pursues real byte
   * parity instead of this file's own simplified deviation.) See
   * Ast_asm.virtual_instr's own comment on why this lives here
   * instead of that shared type, and
   * docs/claude_notes/plan_hello_libc_linking.md for how this was
   * found (stress-testing against goken's real lib_core/libc, e.g.
   * fmt/dofmt.c and the strtol family, which need it). *)
  | CASE of reg
  | BCASE of A.branch_operand
  (*x: [[Ast_asm5.instr]] control-flow instructions cases *)
  | CmpF of A.floatp_precision * freg * freg
  (*e: [[Ast_asm5.instr]] control-flow instructions cases *)

  (* System *)
  (*s: [[Ast_asm5.instr]] system instructions cases *)
  | SWI of int (* value actually unused in Plan 9 and Linux *)
  | RFE (* virtual, sugar for MOVM *)
  (* claude: conditional/plain return ("RET.MI", from real 5c -S
   * output compiling e.g. "if(x<0) return -x;" into a predicated
   * return instead of a branch) -- same "virtual, sugar for X" shape
   * as RFE above, except the real expansion (B(LR) vs a stack-
   * restoring MOVE, depending on frame size) isn't known until
   * Rewrite5.ml's step2 sees the enclosing TEXT's frame size, so it
   * stays a placeholder like Ast_asm.virtual_instr's own plain RET,
   * just carrying a real condition (Ast_asm.virtual_instr has no
   * notion of per-arch conditions, so this couldn't live there). *)
  | CRET
  (*e: [[Ast_asm5.instr]] system instructions cases *)
(*e: type [[Ast_asm5.instr]] *)

(*s: type [[Ast_asm5.arith_opcode]] *)
  and arith_opcode = 
    (* logic *)
    | AND | ORR | EOR
    (* arithmetic *)
    | ADD | SUB   | MUL   | DIV | MOD (* DIV and MOD are virtual *)
    (* claude: unsigned division/modulo -- see this file's own header
     * TODO comment (now resolved) for why these, unlike DIV/MOD, are
     * a xix-only extension with a deliberately different (real
     * hardware SDIV/UDIV, Codegen5.ml's own comment) encoding. *)
    | DIVU | MODU
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
     and move_cond =
       WriteAddressBase (* .W *) | PostOffsetWrite (* .P *)
       (* claude: "MOVW.S R0,R7" -- the classic ARM "test and move"
        * idiom (set NZCV flags from the moved value, for a following
        * predicated instruction like "MOVW.NE ..."), real 5c -S
        * output for e.g. lib_core/libc/fmt/dofmt.c. Mutually exclusive
        * with WriteAddressBase/PostOffsetWrite in practice (this is
        * the plain register-to-register MOVE form, not a memory
        * addressing mode) -- see move_opt_of_flags's own comment and
        * docs/claude_notes/plan_hello_libc_linking.md. *)
       | SetFlags (* .S *)
(*e: type [[Ast_asm5.move_cond]] *)

[@@deriving show]

(* claude: MOVE's own condf-flags decoder, same idea as
 * movm_addr_mode_of_flags above (mechanical bit decode only --
 * like that function, validating which flag combinations are
 * actually legal is left to the grammar action, so it can call
 * Parser_asm.error with position info the same way MOVM's own S/F
 * rejection does). Found stress-testing o5a against goken's real 5c
 * -S output for lib_core/libc/port/memset.c (and friends: memmove/
 * memchr/strchr/strncpy/... all use the same "MOVx.P Rt,off(Rbase)"
 * post-increment copy-loop idiom), which o5a's grammar rejected
 * outright: Parser_asm5.mly's TMOV production used the plain `cond`
 * nonterminal (real ARM condition codes only), not `condf`, even
 * though the lexer already tokenizes ".P"/".W" as TSUF (used by
 * MOVM) -- see move_cond just above and its "MOVW.P autosize(SP),
 * PC" user in linker/Rewrite5.ml's CRET expansion, which builds the
 * same move_option by hand rather than parsing it, so this gap was
 * invisible to any purely hand-constructed-AST testing. Unlike
 * MOVM's PUW (three independent bits), a plain MOVE only ever has
 * ONE addressing mode: post-indexed (.P, "use address, then write
 * back") xor pre-indexed-writeback (.W, "compute address with
 * offset, use it, write it back") xor set-flags (.S, SetFlags's own
 * comment) -- never more than one of the three. *)
let move_opt_of_flags (flags : int) : move_option =
  match flags land sflag_pbit <> 0, flags land sflag_wbit <> 0,
        flags land sflag_sbit <> 0 with
  | false, false, false -> None
  | true,  false, false -> Some PostOffsetWrite
  | false, true,  false -> Some WriteAddressBase
  | false, false, true  -> Some SetFlags
  | _ -> None (* caller must reject any other combination *)

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
  | BCASE opd -> Some opd
  | Arith _ | ArithF _ | MOVWF _ | MOVFW _ | MOVE _ | MOVEF _ | SWAP _
  | Cmp _ | CmpF _ | SWI _ | RFE | MULL _ | MOVM _ | CRET | CASE _ -> None
(*e: function [[Ast_asm5.branch_opd_of_instr]] *)

(*s: function [[Ast_asm5.visit_globals_instr]] *)
let visit_globals_instr (f : global -> unit) (i : instr_with_cond) : unit =
  let mov_operand x =
    match x with
    | Entity (A.Global (x, _)) -> f x
    | Entity (A.Param _ | A.Local _) -> ()
    | Ximm x -> A.visit_globals_ximm f x
    | Imsr _ | Indirect _ | IndirectShift _ | FImsr _ | FCRImsr _
    | PSRImsr _ | RegList _ -> ()
  in
  match fst i with
  | MOVE (_, _, m1, m2) -> mov_operand m1; mov_operand m2
  | MOVEF (_, m1, m2) -> mov_operand m1; mov_operand m2
  | MOVM (_, m1, m2) -> mov_operand m1; mov_operand m2
  (* ocaml-light: | B b | BL b | Bxx (_, b) -> branch_operand b *)
  | B b -> A.visit_globals_branch_operand f b
  | BL b -> A.visit_globals_branch_operand f b
  | Bxx (_, b) -> A.visit_globals_branch_operand f b
  | BCASE b -> A.visit_globals_branch_operand f b
  | Arith _ | ArithF _ | MOVWF _ | MOVFW _ | SWAP _ | Cmp _ | CmpF _ | SWI _
  | RFE | MULL _ | CRET | CASE _ -> ()
(*e: function [[Ast_asm5.visit_globals_instr]] *)
(*e: objects/Ast_asm5.ml *)
