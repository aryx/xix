(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * as published by the Free Software Foundation, with the special
 * exception on linking described in file license.txt.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * license.txt for more details.
 *)
open Common

module A = Ast_asm
open Ast_asm

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Abstract Syntax Tree (AST) for the assembly language supported by 6a/6l
 * (goken's Plan 9 amd64 assembler/linker). I call this language Asm6. See
 * docs/claude_notes/notes_amd64_port_plan.txt for the overall port plan.
 *
 * Real goken C source for this arch is checked out at
 * ~/goken/assemblers/6a (a.h/a.y/lex.c, the grammar) and ~/goken/linkers/6l
 * (obj.c/optab.c/span.c/asm.c, the codegen) -- unlike some of the earlier
 * "read span.c/obj.c for one specific dispatch rule" ports, this one is
 * grounded in that source much more heavily throughout (register/REX/
 * ModRM/SIB encoding, doasm()'s per-opcode Z-code dispatch in span.c) since
 * x86-64 has no real RISC-family analog anywhere else in this project to
 * lean on intuition from.
 *
 * claude: THE central architectural difference from every arch ported so
 * far (ARM32/MIPS/ARM64/RISC-V): amd64 instructions are variable-length
 * (1-15 bytes), not a fixed 4-byte word. See linker/Types.ml's
 * bytes_of_words comment and linker/Execgen.ml's own comment for how the
 * shared executable-writing layer was generalized (byte array text
 * section, not word list) to accommodate this -- confirmed with the user
 * before making that change, since it touches the 4 already-completed,
 * differential-tested ports too (verified zero regressions after).
 *
 * claude: SP is genuinely different here than on every other arch. On
 * ARM32/MIPS/ARM64/RISC-V, Plan9's "SP" token is a purely *virtual*
 * pseudo-register -- real local-variable addressing ("x(SP)") is resolved
 * by the *linker*, once the enclosing TEXT's real frame size is known
 * (Ast_asm.entity's own `Local of symbol option * offset (* SP *)`
 * comment), and the real hardware stack-pointer register has its own,
 * different name on those archs (R13 on ARM32, X31/RSP has no ARM32
 * analog, etc). On amd64, "SP" (and "BP") *is* the real hardware register
 * (D_SP in goken's own 6.out.h, not the separate D_AUTO/D_PARAM virtual
 * address classes) -- confirmed against goken's real 6a/6l: goken's own
 * hello_linux_amd64.s uses bare "SP" as an ordinary arithmetic operand
 * ("SUBQ $16,SP") and "0(SP)"/"8(SP)" as a plain, already-concrete
 * indirect-with-displacement addressing mode (`Indirect` below), needing
 * no linker-side rewriting at all. Only "x(FP)" (goken's D_PARAM) is the
 * traditional Plan9 *virtual* addressing convention here, resolved via
 * `Entity (A.Param ...)` exactly like every other arch -- see Parser_asm6.mly's
 * `reg`/`name`/`pointer` productions for how this plays out grammar-wise
 * (TSP produces both a plain `reg` alternative *and* still feeds `ireg`'s
 * ordinary register-indirect addressing, never `Entity (A.Local ...)`,
 * which this arch's grammar never constructs at all).
 *
 * claude: registers are numbered 0-15 matching goken's own reg[]/regrex[]
 * *encoding* order (AX=0,CX=1,DX=2,BX=3,SP=4,BP=5,SI=6,DI=7,R8-R15=8-15),
 * not the D_AL=0-based enum order 6.out.h itself uses (D_AX=16) -- i.e.
 * already-normalized-for-ModRM/REX values, since that's the only thing
 * Codegen6.ml ever needs them for. R8-R15 *are* wired now (REX.R/.B --
 * see Codegen6.ml's `rex` helper), reachable via the shared "R"+digit
 * lexer rule (no special grammar case needed, unlike the named low
 * registers -- see Parser_asm6.mly).
 *
 * Scope so far (see plan_amd64_port.md): 64-, 32-, 16-, and 8-bit-width
 * (Q/L/W/B-suffixed) integer arithmetic (ADD/SUB/XOR/CMP, immediate-or-
 * register source, register-or-memory destination), move (register/
 * memory/immediate, all combinations MOVQ/MOVL/MOVW/MOVB actually need
 * -- note MOVL/MOVW/MOVB's own immediate-to-*register* form is a
 * genuinely different encoding shape from MOVQ's, see Codegen6.ml),
 * LEAQ (address-of-global, 64-bit only), CALL/JMP (direct to a label,
 * or indirect through a register), short-form (rel8) Jcc, RET,
 * SYSCALL. Double-precision SSE floating point (MOVSD, ADDSD/SUBSD/
 * MULSD/DIVSD, UCOMISD, CVTSQ2SD/CVTTSD2SQ int<->float conversion) --
 * no single-precision (SS-suffixed) forms yet, no float immediates
 * (real amd64 has none either -- confirmed "MOVSD $0,X0" is rejected
 * by real 6a/6l), no x87. No literal pool (none of these instructions
 * need one -- LEAQ's absolute address, any 64-bit immediate that
 * doesn't fit sign-extended-32-bit, and any float value (always via
 * SSE register conversion, never an immediate) are all encoded inline
 * in the instruction stream on this arch, unlike ARM64/ARM32/MIPS/
 * RISC-V's separate pool mechanism).
 *)

(*****************************************************************************)
(* The AST related types *)
(*****************************************************************************)

(* ------------------------------------------------------------------------- *)
(* Operands *)
(* ------------------------------------------------------------------------- *)

type register = A.register (* between 0 and 15 on ARM ... *)
(* claude: original TODO stub's own comment (kept for continuity) --
 * see this file's prelude for the *actual* amd64 register/encoding
 * story (0-15 in goken's reg[]/regrex[] ModRM/REX-ready order). *)
[@@deriving show]

type fregister = A.fregister (* between 0 and 7 *)
[@@deriving show]

type mregister = M of int (* between 0 and 7 *)
[@@deriving show]

(* claude: goken's own D_X0..D_X0+15 (obj.c's reg[]/regrex[] init) --
 * the SSE XMM register file, a separate 16-register bank from `register`
 * above but numbered/REX-extended (X8-X15 need REX.R/.B, exactly like
 * R8-R15) the *same* way -- see Codegen6.ml's `resolved_gen_of_xgen`
 * for why this port reuses the existing GP-register REX/ModRM helpers
 * unchanged rather than duplicating them for a second register file. *)
type xregister = X of int (* between 0 and 15 *)
[@@deriving show]

(* claude: goken's "Yxm"-shaped operand (an XMM register, or memory
 * addressed exactly like `gen`'s own Indirect/Entity, but *never* a GP
 * `register` value) -- used everywhere a real SSE instruction's
 * register-or-memory operand can go (MovF's src/dst, ArithF/CmpF's
 * src). Named `XIndirect`/`XEntity` rather than reusing `gen`'s own
 * `Indirect`/`Entity` constructor names to avoid shadowing them in this
 * shared module. *)
type xgen =
  | XReg of xregister
  | XIndirect of register * A.offset
  | XEntity of A.entity
[@@deriving show { with_path = false }]

type crregister = CR of int (* between 0 and 15 *)
[@@deriving show]

type drregister = DR of int (* between 0 and 7 *)
[@@deriving show]

type trregister = TR of int (* between 0 and 7 *)
[@@deriving show]

(* claude: goken's own `imr`/`imsr`-shaped source operand -- an
 * immediate or a register (never memory) -- used by Arith's source. *)
type imr =
  | Imm of A.integer
  | Reg of register
[@@deriving show { with_path = false }]

(* claude: goken's general "m" operand class (register, register-
 * indirect-with-displacement, or an SB/FP-relative symbolic entity) --
 * used everywhere a real instruction's memory-or-register operand can
 * go (MOVQ's src/dst, Arith's dst). Note this is *not* used for SP: see
 * this file's prelude, "0(SP)" is `Indirect (rSP, offset)` directly, a
 * genuinely concrete addressing mode on this arch, not `Entity (A.Local
 * ...)`. *)
type gen =
  | GReg of register
  | Indirect of register * A.offset
  | Entity of A.entity
[@@deriving show { with_path = false }]

(* ------------------------------------------------------------------------- *)
(* Instructions *)
(* ------------------------------------------------------------------------- *)
type instr =
  (* Arithmetic *)
  (* claude: goken's yaddl/yxorl-shaped 2-operand arithmetic (optab.c):
   * source is an immediate-or-register (`imr`), destination is a
   * register-or-memory (`gen`) -- confirmed against real 6a/6l this is
   * one shared grammar/encoding shape for ADD/SUB/XOR/AND/OR (they all
   * literally share goken's own `yxorl`/`yxorb` tables -- see
   * Codegen6.ml's `arith_ext`/`arith_rr_opcode`). *)
  | Arith of width * arith_opcode * imr * gen
  (* claude: goken's ycmpl-shaped compare (optab.c) -- CMP writes no
   * result, only flags, and (confirmed against real 6a byte output)
   * spells its two operands in the *opposite* role-order from Arith:
   * "CMPQ gen,imr" ("compare gen against imr"), with `gen` -- not
   * `imr` -- landing in the ModRM r/m field either way. Kept as its
   * own constructor (not folded into Arith) since goken's own y-table
   * row order genuinely differs (Yml,Yi8/Yml,Yrl -- gen-first --
   * unlike yaddl's Yi8,Yml/Yrl,Yml -- imr-first). *)
  | Cmp of width * gen * imr
  (* claude: goken's yshl/yshb-shaped shift (optab.c) -- SHL(=SAL)/SHR/
   * SAR, destination a register-or-memory (`gen`), amount either a
   * literal immediate or CX (real x86's only two shift-amount forms;
   * `imr`'s own `Reg` isn't reused here since a *general* register
   * would be wrong -- real amd64 only ever shifts by CL/CX, and
   * goken's own y-table only has a `Ycx`/`Ycl` row, no general `Yrl`
   * one, confirmed: any other register fails at `6l` with
   * "notfound"). A literal `1` is a genuinely different *encoding*
   * from any other immediate (goken's own `Yi1` class, opcode `0xd0`/
   * `0xd1`, no immediate byte at all -- confirmed against real 6a/6l:
   * "SHLQ $1,AX" -> `48 d1 e0`, vs "SHLQ $4,AX" -> `48 c1 e0 04`), so
   * `ShiftImm`'s own codegen clause must special-case that value, same
   * "one AST case, two encodings picked by value" shape `Cvt*`'s
   * width-generic split elsewhere in this file doesn't need but
   * `Move`'s Zclr special case already established the precedent
   * for. *)
  | Shift of width * shift_opcode * shift_amount * gen
  (* claude: goken's ymb_rl/yml_rl-shaped sign/zero-extending "widening
   * move" (optab.c) -- MOVBLSX/MOVBLZX/MOVBQSX/MOVBQZX/MOVWLSX/MOVWLZX/
   * MOVWQSX/MOVWQZX/MOVLQSX/MOVLQZX, real x86's own MOVSX/MOVZX/MOVSXD
   * opcodes (MOVLQZX has no dedicated MOVZX opcode in real x86 -- a
   * plain 32-bit register *write* already implicitly zero-extends to
   * 64 bits -- so goken's own optab.c entry, `{AMOVLQZX,yml_rl,Px,
   * 0x8b}`, just reuses plain MOV's own load opcode. Tempting to alias
   * this to `Move`'s own existing codegen instead of a dedicated
   * `Extend` case -- but `yml_rl`'s single row is *always* the load
   * direction (`Zm_r`, ModRM.reg=dst/ModRM.rm=src), whereas `Move`'s
   * own reg-reg case picks the *store* direction first (see `Move`'s
   * own comment) -- confirmed the hard way: "MOVLQZX AX,BX" ->
   * `8b d8`, not `Move`'s own would-be `89 d8`, a real, if
   * byte-cosmetic, difference caught only by testing the actual reg-
   * reg case, not just reg-mem). *)
  | Extend of extend_opcode * gen * register
  (* claude: goken's yincb/yincl/yincw/yscond-shaped single-operand
   * ModRM-extension-group ops (optab.c) -- NEG/NOT (goken's own
   * `yscond` table, shared with SETcc's own single-`Ymb`-operand shape
   * even though NEG/NOT aren't conditional at all -- just a
   * coincidentally-identical row shape) and INC/DEC (`yincb`/`yincw`/
   * `yincl`). All four share the exact same "opcode, ModRM with a
   * fixed extension digit, no immediate at all" encoding (goken's
   * `Zo_m`, the very same Z-code `Shift`'s own shift-by-1/shift-by-CL
   * cases already use above) -- confirmed against real 6a/6l: "NEGQ
   * AX" -> `48 f7 d8`, "INCQ AX" -> `48 ff c0`. *)
  | Unary of width * unary_opcode * gen

  (* Memory *)
  (* claude: goken's ymovq/ymovl-shaped move (optab.c) -- source is
   * either a `gen` (register/memory/entity) or an immediate/address
   * (`A.ximm`, reusing the same shared type ARM64's own Move already
   * uses), destination is always a `gen` (real amd64 MOV can never
   * write to an immediate, obviously). *)
  | Move of width * (gen, A.ximm) Either_.t * gen
  (* claude: goken's Zaut_r/"built-in LEAQ" case (optab.c's ymovq table
   * has its own Zaut_r row just for this, span.c's doasm() `case
   * Zaut_r` comment literally says "leal" -- LEA is encoding-wise its
   * own opcode (0x8d), not a MOV variant, even though source-level it
   * reads like one; kept as a separate constructor to match, rather
   * than folding into Move). Only the "address of a global" form is
   * wired (goken's own D_ADDR-with-D_EXTERN/D_STATIC index, see
   * Codegen6.ml) -- address-of-local/-param is a real, separate goken
   * feature (the *actual* Zaut_r case: "leal" is used for taking the
   * address of an *auto* local) not needed by hello_linux_amd64.s and
   * not wired here. *)
  | Lea of A.global * A.offset * register

  (* Control flow *)
  (* claude: goken's ycall-shaped CALL (optab.c) -- direct (to a label,
   * goken's own D_EXTERN/D_STATIC branch target, opcode 0xe8 rel32)
   * only; the indirect-through-register/memory form (opcode 0xff /2)
   * is a separate y-class row, not wired yet. *)
  | Call of A.branch_operand
  (* claude: goken's yjmp-shaped unconditional jump -- direct (to a
   * label) only, same indirect-form gap as Call. *)
  | Jmp of A.branch_operand
  (* claude: goken's yjcond-shaped conditional jump -- always to a
   * label (goken's own Ybr class, never register-indirect). *)
  | Jcc of condition * A.branch_operand
  | Ret

  (* Floating point *)
  (* claude: goken's yxmov-shaped MOVSD (optab.c) -- double-precision
   * only (see this file's own "Scope so far" note); note SSE reg-reg
   * moves take the *load*-shaped row (`Zm_r_xm`, opcode 0x10) ahead of
   * the store-shaped one (`Zr_m_xm`, opcode 0x11) in goken's own
   * `yxmov` table -- the *opposite* row order from `ymovq`/`ymovl`'s
   * own `Zr_m`-before-`Zm_r`, confirmed against real 6a/6l ("MOVSD
   * X0,X1" -> `f2 0f 10 c8`, the load opcode, even though both
   * operands are plain registers) -- so `MovF`'s own codegen clauses
   * must be ordered opposite from `Move`'s (see Codegen6.ml). No
   * float-immediate form exists at all (confirmed: real 6a rejects
   * "MOVSD $0,X0" outright), matching every other arch's own choice
   * to skip float immediates (e.g. Ast_asm7.ml's `FArith` comment).
   * `A.floatp_precision` (shared with Ast_asmv.ml/Ast_asmi.ml/
   * Ast_asm7.ml) picks MOVSD vs MOVSS -- goken's own `yxmov` table is
   * identical for both, just `Pf2` vs `Pf3` (see Codegen6.ml). *)
  | MovF of A.floatp_precision * xgen * xgen
  (* claude: goken's yxm-shaped dyadic SSE arithmetic (optab.c) --
   * ADDSD/SUBSD/MULSD/DIVSD (and their SS-suffixed siblings, same
   * opcode bytes, just `Pf3` instead of `Pf2`), real x86's own
   * 2-operand in-place shape (`dst := dst op src`, no 3-operand form
   * the way VFP/NEON have) -- confirmed against real 6a/6l ("ADDSD
   * X1,X0" -> `f2 0f 58 c1`, ModRM.reg=X0 (dst), ModRM.rm=X1 (src)).
   * `arithf_opcode` itself stays precision-generic (matching
   * Ast_asmi.ml's own `ArithF of (arithf_opcode * A.floatp_precision)
   * * ...` convention: one AST case per *operation*, precision
   * threaded alongside rather than doubling the opcode count). *)
  | ArithF of arithf_opcode * A.floatp_precision * xgen * xregister
  (* claude: goken's yxcmp-shaped UCOMISD/UCOMISS (optab.c) -- unlike
   * ADDSD/etc, UCOMISD's own real prefix is `Pe` (0x66), *not* `Pf2`
   * (confirmed against real optab.c and real 6a/6l byte output:
   * "UCOMISD X1,X0" -> `66 0f 2e c1`, no `f2` byte at all) -- and
   * UCOMISS's is `Pm`, i.e. *no* legacy prefix at all (confirmed:
   * "UCOMISS X1,X0" -> `0f 2e c1`), a third, different prefix story
   * from every other SSE instruction here. Sets integer EFLAGS
   * (ZF/PF/CF) the same way an unsigned integer CMP does, so this
   * port's *existing* unsigned `Jcc` conditions (JCS/JCC/JHI/JLS) are
   * reused as-is for a float branch -- no separate float condition
   * type needed (matching real x86 usage: a NaN operand sets PF as
   * well as ZF+CF, which this port doesn't attempt to special-case,
   * same "don't model IEEE unordered comparisons precisely" scope
   * choice ARM64's own `FCmp` comment makes). *)
  | CmpF of A.floatp_precision * xgen * xregister
  (* claude: goken's yxcvlf/yxcvqf-shaped CVTSQ2SD/CVTSQ2SS (optab.c) --
   * 64-bit integer (register or memory, goken's own `Yml`) to
   * single- or double-precision float, confirmed against real 6a/6l
   * both need REX.W regardless of precision (goken's `Pw` prefix,
   * alongside `Pf2`/`Pf3`) -- "CVTSQ2SD AX,X3" -> `f2 48 0f 2a d8`,
   * "CVTSQ2SS AX,X3" -> `f3 48 0f 2a d8`. The 32-bit-int forms
   * (`CVTSL2SD`/`CVTSL2SS`, no REX.W) aren't wired -- not needed by
   * this checkpoint's own fixtures, and every GP register in this
   * port's own scope is already treated as 64-bit-wide by convention
   * (see `width`'s own `Q_` case). *)
  | CvtIntToF of A.floatp_precision * gen * xregister
  (* claude: goken's yxcvfq-shaped CVTTSD2SQ/CVTTSS2SQ (optab.c) -- the
   * reverse conversion, *truncating* (not rounding -- real x86 also
   * has separate, non-truncating CVTSD2SQ/CVTSS2SQ this port doesn't
   * wire, matching ARM64's own choice to skip the round-to-nearest
   * FCVTNS variant and only carry FCVTZS). Also REX.W-forced
   * regardless of precision, confirmed: "CVTTSD2SQ X3,BX" ->
   * `f2 48 0f 2c db`, "CVTTSS2SQ X3,BX" -> `f3 48 0f 2c db`. *)
  | CvtFToInt of A.floatp_precision * xgen * register

  (* System *)
  | Syscall

  and arith_opcode = ADD | SUB | XOR | AND | OR
  (* claude: goken's own real amd64 shift opcodes -- SHL and SAL are
   * genuine aliases in real x86 (same opcode, ext=4, both spellings
   * accepted by real 6a: `ASHLL`/`ASALL` are two separate optab.c
   * entries with byte-for-byte identical rows) -- kept as one AST
   * case here (`SHL`) rather than two, since nothing downstream ever
   * needs to distinguish which spelling the user wrote; both mnemonic
   * spellings map to it in Parse_asm6.ml. *)
  and shift_opcode = SHL | SHR | SAR
  and shift_amount = ShiftImm of int | ShiftReg of register

  (* claude: goken's own real amd64 sign/zero-extend mnemonics. *)
  and extend_opcode =
    | MOVBLSX | MOVBLZX | MOVBQSX | MOVBQZX
    | MOVWLSX | MOVWLZX | MOVWQSX | MOVWQZX
    | MOVLQSX | MOVLQZX

  and unary_opcode = NEG | NOT | INC | DEC
  (* claude: goken's own yxm table is shared verbatim across ADDSD/
   * SUBSD/MULSD/DIVSD *and* their SS-suffixed siblings (only the final
   * opcode byte differs per operation, not per precision -- see
   * Codegen6.ml's `arithf_opcode_byte`), same "one AST case per real
   * mnemonic family" choice `arith_opcode` above already makes. *)
  and arithf_opcode = FADD | FSUB | FMUL | FDIV

  (* claude: operand width, shared by Arith/Cmp/Move -- Q_ (64-bit,
   * REX.W set), L_ (32-bit, no REX.W -- the *default* operand size in
   * long mode, confirmed against real 6a: "ADDL BX,AX" needs no
   * prefix byte at all when no R8-R15 register is involved), W_
   * (16-bit, goken's own "Pe" -- a mandatory 0x66 operand-size-
   * override prefix *before* any REX byte, no REX.W; same opcodes as
   * L_ throughout, confirmed against real 6a) -- see Codegen6.ml's
   * `rex_opt`/`prefix66` -- and B_ (8-bit, goken's own "Pb" prefix,
   * *not* an actual prefix byte but a signal to reinterpret whichever
   * general register the grammar names as its own low byte -- see
   * Codegen6.ml's `bytereg`-equivalent comment). B_ has genuinely
   * different opcode numbers from L_/Q_/W_'s shared ones (confirmed
   * against goken's own optab.c: e.g. ADDB's reg-reg opcode is 0x00,
   * not L_'s 0x01), and a real REX-forcing quirk for SP/BP/SI/DI (see
   * Codegen6.ml) -- legacy AH/BH/CH/DH forms (reachable in real amd64
   * only when *no* REX byte at all is present) aren't wired, since
   * this port's register model has no distinct token for them and
   * nothing needs them. *)
  and width = Q_ | L_ | W_ | B_

  (* claude: goken's real amd64 condition codes -- EQ/NE plus signed
   * (JLT/JGE/JGT/JLE) and unsigned (JCS/JCC/JHI/JLS) variants of
   * less/greater-or-equal/greater/less-or-equal, same sign-parameterized
   * shape as Ast_asm5.ml/Ast_asm7.ml's own `condition` type (ARM's
   * identical signed-vs-unsigned split for its own Bxx family) --
   * confirmed against goken's real optab.c (AJEQ/AJNE/AJLT/AJGE/AJGT/
   * AJLE/AJCS/AJCC/AJHI/AJLS). *)
  and condition =
    | EQ | NE
    | LT of A.sign | GE of A.sign | GT of A.sign | LE of A.sign

[@@deriving show { with_path = false }]

(* reserved by the linker/calling convention (include/objexec/6.out.h) *)
let rAX  = R 0
let rSP  = R 4  (* D_SP -- see this file's prelude: real register here,
                  * not a virtual pseudo-register like every other arch *)
let rDI  = R 7
(* claude: goken's own REGTMP for amd64 (6.out.h: REGTMP = D_DI) --
 * kept for parity with every other arch's `rTMP`/Arch_linker wiring
 * even though nothing in this first checkpoint's Rewrite6.ml/
 * Codegen6.ml generates code that needs a scratch register yet. *)
let rTMP = rDI

let nb_registers = 16
let nb_fregisters = 8

(* ------------------------------------------------------------------------- *)
(* Program *)
(* ------------------------------------------------------------------------- *)

(* for ocaml-light to work without deriving *)
let show_program _ = "NO DERIVING"
[@@warning "-32"]
let show_line _ = "NO DERIVING"
[@@warning "-32"]

type line = instr A.line
[@@deriving show]

type program = instr A.program
[@@deriving show]

(*****************************************************************************)
(* Extractors/Visitors *)
(*****************************************************************************)

let branch_opd_of_instr (instr : instr) : A.branch_operand option =
  match instr with
  | Call opd -> Some opd
  | Jmp opd -> Some opd
  | Jcc (_, opd) -> Some opd
  | Arith _ | Cmp _ | Shift _ | Extend _ | Unary _ | Move _ | Lea _ | Ret | Syscall -> None
  | MovF _ | ArithF _ | CmpF _ | CvtIntToF _ | CvtFToInt _ -> None

let visit_globals_instr (f : global -> unit) (i : instr) : unit =
  let gen_operand x =
    match x with
    | Entity (A.Global (x, _)) -> f x
    | Entity (A.Param _ | A.Local _) -> ()
    | GReg _ | Indirect _ -> ()
  in
  let xgen_operand x =
    match x with
    | XEntity (A.Global (x, _)) -> f x
    | XEntity (A.Param _ | A.Local _) -> ()
    | XReg _ | XIndirect _ -> ()
  in
  match i with
  | Move (_, x1, gen2) ->
      (match x1 with
      | Either.Left gen1 -> gen_operand gen1
      | Either.Right ximm1 -> A.visit_globals_ximm f ximm1
      );
      gen_operand gen2
  | Lea (g, _, _) -> f g
  | Call b | Jmp b | Jcc (_, b) -> A.visit_globals_branch_operand f b
  | Arith (_, _, _, gen1) -> gen_operand gen1
  | Cmp (_, gen1, _) -> gen_operand gen1
  | Shift (_, _, _, gen1) -> gen_operand gen1
  | Extend (_, gen1, _) -> gen_operand gen1
  | Unary (_, _, gen1) -> gen_operand gen1
  | MovF (_, x1, x2) -> xgen_operand x1; xgen_operand x2
  | ArithF (_, _, x1, _) -> xgen_operand x1
  | CmpF (_, x1, _) -> xgen_operand x1
  | CvtIntToF (_, g1, _) -> gen_operand g1
  | CvtFToInt (_, x1, _) -> xgen_operand x1
  | Ret | Syscall -> ()
