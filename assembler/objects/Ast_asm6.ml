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
 * docs/claude_notes/amd64_port.md for the overall port plan.
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
 * Scope (complete for this port's own feature set -- see amd64_port.md
 * for the full writeup): 64-, 32-, 16-, and 8-bit-width (Q/L/W/B-
 * suffixed) integer ADD/SUB/XOR/AND/OR/CMP/TEST (immediate-or-register
 * source, register-or-memory destination), SHL(=SAL)/SHR/SAR shifts,
 * sign/zero-extending "widening move" (MOVBLSX/MOVBLZX/etc), NEG/NOT/
 * INC/DEC, MUL/IMUL/DIV/IDIV (single-operand and IMUL's own 2-operand
 * form) plus CWD/CDQ/CQO, CMPXCHG+LOCK, move (register/memory/
 * immediate, all combinations MOVQ/MOVL/MOVW/MOVB actually need --
 * note MOVL/MOVW/MOVB's own immediate-to-*register* form is a
 * genuinely different encoding shape from MOVQ's, see Codegen6.ml),
 * LEAQ (address-of-global, 64-bit only), CALL/JMP (direct to a label,
 * or indirect through a register), short-form (rel8) Jcc, RET,
 * SYSCALL. Single- and double-precision SSE floating point (MOVSD/
 * MOVSS, ADDSD/SUBSD/MULSD/DIVSD + SS siblings, UCOMISD/UCOMISS, both
 * int widths of CVTS{L,Q}2S{D,S}/CVTTS{D,S}2S{L,Q}, CVTSD2SS/
 * CVTSS2SD, XORPD/XORPS's self-clear idiom, a raw GP<->XMM MOVQ
 * bit-copy, PSLLQ) -- no float immediates (real amd64 has none either
 * -- confirmed "MOVSD $0,X0" is rejected by real 6a/6l), no x87. No
 * literal pool (none of these instructions need one -- LEAQ's absolute
 * address, any 64-bit immediate that doesn't fit sign-extended-32-bit,
 * and any float value (always via SSE register conversion, never an
 * immediate) are all encoded inline in the instruction stream on this
 * arch, unlike ARM64/ARM32/MIPS/RISC-V's separate pool mechanism).
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
  (* claude: same scaled-index case as `gen`'s own IndirectScaled/
   * EntityScaled above (e.g. real fmt/fltfmt.c's own "MOVSD
   * pows10<>+0(SB)(AX*8),X0" -- indexing a table of double constants). *)
  | XIndirectScaled of register * A.offset * register * int
  | XEntityScaled of A.entity * register * int
  (* claude: real x86 has no opcode to move/combine an immediate float
   * directly into an XMM register -- goken's own linkers/6l/obj.c
   * (AMOVSD/AMULSD/etc.'s D_FCONST preprocessing case) handles a
   * literal float source by synthesizing a hidden DATA symbol (named
   * by the float's own IEEE754 bit pattern) and rewriting the
   * instruction to reference it instead, i.e. an auto-generated
   * literal pool. This port does the same in Rewrite6.ml, *before*
   * Codegen6.ml ever runs -- `XFloatImm` only exists between parsing
   * and that rewrite pass; Codegen6.ml's `gen_of_xgen` raises
   * Impossible if it ever sees one, since that would mean Rewrite6.ml
   * was skipped or missed a case. Found stress-testing real
   * lib_core/libc (fmt/fltfmt.c's own "MOVSD $(1.0e+00),X0"). *)
  | XFloatImm of float
  (* claude: same named-local-against-SP case as `gen`'s own LocalSP/
   * LocalSPScaled above -- see that comment. *)
  | XLocalSP of A.offset
  | XLocalSPScaled of A.offset * register * int
[@@deriving show { with_path = false }]

type crregister = CR of int (* between 0 and 15 *)
[@@deriving show]

type drregister = DR of int (* between 0 and 7 *)
[@@deriving show]

type trregister = TR of int (* between 0 and 7 *)
[@@deriving show]

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
  (* claude: real x86 SIB scaled-index addressing -- "(BX)(CX*4)"
   * (register base) or "tab<>+0(SB)(CX*8)" (SB-relative global base,
   * no base register in the SIB byte itself, same disp32-only
   * convention as plain `Entity (A.Global ...)` -- see Codegen6.ml's
   * `RAbs` comment). Confirmed against goken's real assemblers/6a/a.y
   * (`omem`'s "con '(' LLREG '*' con ')'" / "'(' LLREG ')' '(' LLREG
   * '*' con ')'" rows and `nmem`'s "nam '(' LLREG '*' con ')'" row) --
   * genuinely common in real fmt/utf code for indexing an array by a
   * loop variable (e.g. fmt/dofmt.c's own "LEAQ (BX)(CX*1),AX"). Kept
   * as two separate constructors (not a `register option` field on
   * `Indirect`/`Entity`) so every existing non-indexed match arm stays
   * exhaustive without a wildcard. *)
  | IndirectScaled of register * A.offset * register * int (* base, offset, index, scale *)
  | EntityScaled of A.entity * register * int (* entity, index, scale *)
  (* claude: a *named* local variable against SP (e.g. real fmt/
   * vfprint.c's own "LEAQ f+-104(SP),AX" -- $400-frame vfprint's own
   * on-stack Fmt struct), genuinely NOT the same addressing mode as
   * the bare, unlabeled "N(SP)" form `Indirect (rSP, N)` already
   * covers (that one is always a small, non-negative, purely hardware-
   * SP-relative offset, used only for marshaling an *outgoing* call's
   * own arguments at the very bottom of the frame). Confirmed against
   * real 6a/6l (vfprint's own real bytes: "f+-104(SP)" with a real
   * $400 frame assembles to "lea 0x128(%rsp),%rax", i.e. hardware
   * offset 400+(-104)=296=0x128, NOT the raw -104 this port's earlier,
   * unverified version used) -- goken's own convention here is the
   * same "pseudo-SP" scheme Go's own assembler is famous for: a
   * *named* "name+N(SP)" addresses "N bytes above the TOP of the
   * local frame" (autosize+N), while a bare "N(SP)" addresses
   * "N bytes above the hardware SP" directly (no autosize bias) --
   * two genuinely different addressing modes that happen to share the
   * same source syntax shape. Resolved in Codegen6.ml's own
   * `resolve_gen`, the same "needs env.autosize, not knowable at parse
   * time" deferral every other frame-relative case here already uses
   * (see `Entity (A.Local ...)`'s own comment). *)
  | LocalSP of A.offset
  | LocalSPScaled of A.offset * register * int (* offset, index, scale *)
[@@deriving show { with_path = false }]

(* claude: goken's own `imr`/`imsr`-shaped source operand -- an
 * immediate, a register, or (confirmed against 6l/optab.c's yaddl:
 * "Yml,Yrl,Zm_r,1") a memory operand -- used by Arith's source.
 * `Mem`'s own payload is never `GReg` in practice (the grammar routes a
 * bare register through `Reg` instead), but reusing `gen` wholesale
 * avoids a fourth near-duplicate memory-operand type. *)
type imr =
  | Imm of A.integer
  | Reg of register
  | Mem of gen
  (* claude: goken's real "$name(SB)" address-of-global immediate
   * (D_ADDR, same shape as `A.ximm`'s own `Address` -- see that type's
   * comment) -- confirmed against real fmt/fmt.c's own "CMPQ
   * DX,$fmtalloc<>+1032(SB)" (comparing a moving pointer against the
   * end of a static allocation pool). Non-PIE: the address is a fixed
   * link-time constant, resolved the same way `Lea`'s own Global case
   * is (see Codegen6.ml's Cmp/Addr comment for why it still needs the
   * same lazy resolution as Lea, even though it's not a Lea). *)
  | Addr of A.entity
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
  (* claude: goken's ytestl/ytestb-shaped TEST (optab.c) -- like CMP,
   * writes no result, only flags (bitwise AND this time, not
   * subtraction), but spells its operands in yet a *third* role-order:
   * "TESTQ Rs,gen" with the *register* written first landing in
   * ModRM.reg, `gen` second landing in ModRM.rm -- confirmed against
   * real 6a/6l: "TESTL BX,CX" -> `85 d9` (reg=BX, rm=CX). Same
   * B_-is-one-less opcode pair as everything else here (`0x85` vs
   * `0x84`). Only the register-vs-register/memory form is wired
   * (real amd64's own TEST-with-immediate form has no imm8 row at all
   * -- confirmed reading `ytestl`, unlike CMP/Arith -- and no fixture
   * here needs it, so it isn't wired, matching the imm32 gaps
   * elsewhere in this file). *)
  | Test of width * register * gen
  (* claude: goken's yrl_ml/yrb_mb-shaped CMPXCHG (optab.c) -- real
   * x86's own atomic-CAS-friendly compare-and-exchange: "CMPXCHGQ
   * Rs,gen" compares the implicit accumulator (AX) against `gen`; if
   * equal, `gen := Rs` (ZF=1); else `AX := gen` (ZF=0). Same role
   * order as `Arith`'s own `Reg` case and `Move`'s own store clause
   * (`Rs` in ModRM.reg, `gen` in ModRM.rm) -- confirmed against real
   * 6a/6l: "CMPXCHGL CX,0(BX)" -> `0f b1 0b`. Found directly in
   * goken's own hand-written amd64 assembly (a `cas()` primitive,
   * always paired with the `Lock` prefix below on the preceding
   * source line). *)
  | CmpXchg of width * register * gen
  (* claude: goken's own real amd64 LOCK prefix (optab.c: `{ALOCK,
   * ynone, Px, 0xf0}`) -- spelled as its own standalone pseudo-
   * instruction on the line *before* the one it modifies (real x86's
   * own LOCK is a legacy prefix byte, not a separate opcode, but
   * goken's own assembler/linker never actually ties the two
   * together -- it just emits the bare `0xf0` byte as this "instruction"'s
   * own complete encoding and relies on x86 hardware reading it as a
   * prefix for whatever follows, exactly like this port's own
   * `Codegen6.gen` byte-concatenation already does for free). Not
   * validated against what follows, matching goken's own laxness. *)
  | Lock
  (* claude: goken's own `ymovq` table has extra rows beyond the plain-
   * integer ones every other width's own move table has: a *raw*
   * GP<->XMM bit-copy (real x86's own MOVQ xmm,r/m64 / MOVQ r/m64,xmm,
   * opcode `66 REX.W 0F 6E`/`0F 7E` -- genuinely different from
   * `CvtIntToF`/`CvtFToInt`, which *convert* a value, not just move
   * its raw bits). Found directly in goken's own hand-written amd64
   * assembly (`tests/s/regressions/amd64_psllq.s`'s own `MOVQ AX,X0`/
   * `MOVQ X0,DI`) -- confirmed real 6a rejects the same shape for
   * MOVL/MOVW/MOVB (their own `ymovl`/`ymovw`/`ymovb` tables have no
   * XMM rows at all), so this is `MOVQ`-only, two dedicated
   * constructors rather than folding into the existing `Move`/`MovF`
   * (whose own operand types don't mix a GP `gen` with an `xregister`
   * on the same side the way this needs). Confirmed against real
   * 6a/6l: "MOVQ AX,X0" -> `66 48 0f 6e c0`, "MOVQ X0,DI" ->
   * `66 48 0f 7e c7` (the `0x66` prefix always comes before REX.W,
   * same ordering rule as every other prefixed instruction here). *)
  | MovQToXmm of gen * xregister
  | MovQFromXmm of xregister * gen
  (* claude: goken's yps-shaped PSLLQ (optab.c) -- real x86's SSE2
   * packed-integer shift, only the shift-by-immediate form is wired
   * (found directly in `tests/s/regressions/amd64_psllq.s`, a real
   * regression test for a genuine goken bug: `APSLLQ`'s own reg,imm8
   * row once had the wrong opcode byte, `0x7e` -- which is MOVD/MOVQ,
   * not a shift -- instead of `0x73`). No REX at all needed (goken's
   * own `Py` prefix contributes nothing itself; the real `0x66` comes
   * from an *embedded* prefix byte inside the row's own op array, the
   * same mechanism `CvtIntToF`'s `Pf2`/`Pf3` embedding uses) --
   * confirmed: "PSLLQ $4,X0" -> `66 0f 73 f0 04`. *)
  | PsllQXmm of xregister * int
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
  (* claude: goken's ydivl/ydivb-shaped single-operand multiply/divide
   * (optab.c) -- MUL/IMUL/DIV/IDIV, real x86's own implicit-AX(:DX)
   * shape (`AX := AX op gen` for MUL/IMUL, `DX:AX := DX:AX / gen`
   * remainder-in-DX for DIV/IDIV -- goken's grammar only ever spells
   * the divisor/multiplicand, never AX/DX). Same `Zm_o` encoding shape
   * as `Unary`'s own `Zo_m` (opcode + ModRM with a fixed extension
   * digit, no immediate), just with the operand playing goken's
   * *`from`* role instead of `to` -- confirmed against real 6a/6l:
   * "IDIVQ BX" -> `48 f7 fb`. *)
  | MulDiv of width * muldiv_opcode * gen
  (* claude: goken's yimul-shaped 2-operand IMUL (optab.c) -- real
   * x86's own "IMUL r,r/m" form (opcode `0x0f 0xaf`, ModRM.reg=dst,
   * ModRM.rm=src -- the *load*-shaped role assignment, same as
   * `Move`'s own load clause), confirmed against real 6a/6l: "IMULL
   * BX,AX" -> `0f af c3`. The 3-operand immediate forms (goken's own
   * `Zib_rr`/`Zil_rr` rows, "IMUL $imm,Rd" with an implicit src=dst)
   * aren't wired -- not needed by this checkpoint's own fixture, and
   * genuinely rarer in real 6c output than the plain 2-operand form. *)
  | Imul2 of width * gen * register
  (* claude: goken's own real amd64 CWD/CDQ/CQO -- sign-extends AX
   * into DX:AX at 16/32/64-bit width (the standard prep step before
   * IDIV, confirmed used directly ahead of it in real 6c output).
   * Nullary, same shape as `Ret`/`Syscall` below -- kept as three
   * separate constructors rather than threading a `width` through one
   * (the way `Move`/`Arith`/etc do) since B_ has no equivalent
   * instruction at all here (real amd64's own CBW plays that role,
   * not wired -- a different opcode family, not just a different
   * prefix on this same one). *)
  | Cwd | Cdq | Cqo

  (* Memory *)
  (* claude: goken's ymovq/ymovl-shaped move (optab.c) -- source is
   * either a `gen` (register/memory/entity) or an immediate/address
   * (`A.ximm`, reusing the same shared type ARM64's own Move already
   * uses), destination is always a `gen` (real amd64 MOV can never
   * write to an immediate, obviously). *)
  | Move of width * (gen, A.ximm) Either_.t * gen
  (* claude: goken's real LEA (optab.c's `ym_rl` row, opcode 0x8d) --
   * *any* memory operand (register-indirect, scaled-index, SB-relative
   * global, or FP-relative local), never an immediate or a bare
   * register (real 6a's own `ym` class excludes both); kept as its own
   * constructor rather than folding into `Move` since LEA's own opcode
   * (0x8d) and semantics (compute the address, don't dereference it)
   * are genuinely distinct, not just another Move row. Confirmed
   * against real 6a/6l stress-testing lib_core/libc's own
   * "LEAQ (BX)(CX*1),AX" (fmt/dofmt.c) and "LEAQ tab1<>+0(SB)(CX*1),AX"
   * (fmt/strtod.c) -- this port's earlier version only wired the
   * "address of a global, no index" case (goken's own D_ADDR-with-
   * D_EXTERN/D_STATIC), see Codegen6.ml's own Lea comment for why the
   * global case still needs its own lazy/forward-reference-safe
   * resolution while every other `gen` shape doesn't. *)
  | Lea of gen * register

  (* Control flow *)
  (* claude: goken's ycall-shaped CALL (optab.c) -- direct (to a label,
   * goken's own D_EXTERN/D_STATIC branch target, opcode 0xe8 rel32)
   * only; the indirect-through-register/memory form (opcode 0xff /2)
   * is a separate y-class row, not wired yet. *)
  | Call of A.branch_operand
  (* claude: goken's yjmp-shaped unconditional jump -- direct (to a
   * label) only, same indirect-form gap as Call. Real amd64 (confirmed
   * against goken's own optab.c: AJMP's row lists both 0xeb and 0xe9)
   * has *two* encodings -- short (rel8, 2 bytes) and near (rel32, 5
   * bytes) -- and real 6l picks whichever fits, like any assembler's
   * branch relaxation. The trailing `bool ref` is that decision,
   * `false` until Layout6.ml's own fixed-point relaxation pass mutates
   * it (see that file's own comment) -- a real `ref`, not a plain
   * `bool`, so Layout6.ml can flip it in place without reconstructing
   * this instruction (mirroring `A.branch_operand`'s own established
   * "resolved late, in place" convention). *)
  | Jmp of A.branch_operand * bool ref
  (* claude: goken's yjcond-shaped conditional jump -- always to a
   * label (goken's own Ybr class, never register-indirect). Same
   * short-vs-near relaxation story as `Jmp` above. *)
  | Jcc of condition * A.branch_operand * bool ref
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
   * must be ordered opposite from `Move`'s (see Codegen6.ml). A
   * float-immediate source *is* real (confirmed against real
   * lib_core/libc, e.g. fmt/fltfmt.c's own "MOVSD $(1.0e+00),X0") --
   * see `xgen`'s own `XFloatImm` comment for why it's still not a
   * direct hardware operand (real amd64 has no such opcode at all;
   * goken's real 6l synthesizes a hidden DATA symbol for it instead,
   * which this port's Rewrite6.ml mirrors).
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
  (* claude: goken's yxcvlf/yxcvqf-shaped CVTS{L,Q}2S{D,S} (optab.c) --
   * a 32- or 64-bit integer (register or memory, goken's own `Yml`) to
   * single- or double-precision float. Only the int width (`Q_`/`L_`
   * -- REX.W is forced for `Q_`, never set for `L_`, confirmed
   * against real 6a/6l: "CVTSQ2SD AX,X3" -> `f2 48 0f 2a d8` vs
   * "CVTSL2SD AX,X0" -> `f2 0f 2a c0`, no REX at all) changes the
   * encoding; float precision only ever picks the `Pf2`/`Pf3` prefix,
   * same as everywhere else in this section. *)
  | CvtIntToF of width (* Q_ or L_ only *) * A.floatp_precision * gen * xregister
  (* claude: goken's yxcvfq/yxcvfl-shaped CVTTS{D,S}2S{Q,L} (optab.c) --
   * the reverse conversion, *truncating* (not rounding -- real x86
   * also has separate, non-truncating CVTSD2SI/CVTSS2SI this port
   * doesn't wire, matching ARM64's own choice to skip the round-to-
   * nearest FCVTNS variant and only carry FCVTZS). Same `Q_`/`L_`-
   * only REX.W split as `CvtIntToF`, confirmed: "CVTTSD2SQ X3,BX" ->
   * `f2 48 0f 2c db` vs "CVTTSD2SL X0,BX" -> `f2 0f 2c d8`. *)
  | CvtFToInt of width (* Q_ or L_ only *) * A.floatp_precision * xgen * register
  (* claude: goken's yxm-shaped CVTSD2SS/CVTSS2SD (optab.c) -- real
   * x86's own opcode `0x5a` handles *both* directions, disambiguated
   * purely by legacy prefix (`Pf2`=double source, `Pf3`=single
   * source) -- confirmed against real 6a/6l: "CVTSD2SS X0,X2" ->
   * `f2 0f 5a d0`, "CVTSS2SD X1,X3" -> `f3 0f 5a d9`. The precision
   * carried here is the *source*'s (unlike every other `A.floatp_
   * precision` use in this file, which names the operation's own
   * nominal precision) -- `sse_prefix` doesn't care either way, it's
   * purely a prefix-selector, but this is worth flagging since it's
   * the one place the convention flips. *)
  | CvtFPrec of A.floatp_precision (* source precision *) * xgen * xregister
  (* claude: goken's yxm-shaped XORPD/XORPS (optab.c) -- only the
   * self-XOR-to-zero idiom is wired (confirmed real, used directly in
   * 6c-compiled float negation: "XORPD X0,X0" then "SUBSD X1,X0" for
   * "0.0 - X1"), mirroring the existing GP-register `Zclr` special
   * case rather than a general 2-register packed-XOR this port has no
   * other use for. XORPD's own prefix is `Pe` (0x66); XORPS's is `Pm`
   * (no real prefix byte at all, just the `0x0f` escape) -- confirmed:
   * "XORPD X4,X4" -> `66 0f 57 e4`, "XORPS X5,X5" -> `0f 57 ed`. *)
  | XorClearF of A.floatp_precision * xregister

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
  and muldiv_opcode = MUL_ | IMUL_ | DIV_ | IDIV_
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
  | Jmp (opd, _) -> Some opd
  | Jcc (_, opd, _) -> Some opd
  | Arith _ | Cmp _ | Test _ | CmpXchg _ | Lock | Shift _ | Extend _ | Unary _ | MulDiv _ | Imul2 _
  | Cwd | Cdq | Cqo | Move _ | Lea _ | Ret | Syscall -> None
  | MovF _ | ArithF _ | CmpF _ | CvtIntToF _ | CvtFToInt _ | CvtFPrec _ | XorClearF _ -> None
  | MovQToXmm _ | MovQFromXmm _ | PsllQXmm _ -> None

let visit_globals_instr (f : global -> unit) (i : instr) : unit =
  let gen_operand x =
    match x with
    | Entity (A.Global (x, _)) -> f x
    | Entity (A.Param _ | A.Local _) -> ()
    | EntityScaled (A.Global (x, _), _, _) -> f x
    | EntityScaled ((A.Param _ | A.Local _), _, _) -> ()
    | GReg _ | Indirect _ | IndirectScaled _ | LocalSP _ | LocalSPScaled _ -> ()
  in
  let xgen_operand x =
    match x with
    | XEntity (A.Global (x, _)) -> f x
    | XEntity (A.Param _ | A.Local _) -> ()
    | XEntityScaled (A.Global (x, _), _, _) -> f x
    | XEntityScaled ((A.Param _ | A.Local _), _, _) -> ()
    | XReg _ | XIndirect _ | XIndirectScaled _ | XFloatImm _
    | XLocalSP _ | XLocalSPScaled _ -> ()
  in
  (* claude: `imr`'s own memory/address cases (Arith's source, Cmp's
   * second operand) -- found the hard way (Not_found at link time,
   * resolve_global_addr): a global reached *only* through one of
   * these (e.g. real fmt/fmt.c's own "CMPQ DX,$fmtalloc<>+1032(SB)")
   * still needs Load.ml's own `process_global` to run on it (sets its
   * `priv` field, registers it in the symbol table) -- skipping this
   * doesn't crash outright when the same global is *also* reached some
   * other way in the same file (the other reference's `process_global`
   * call already registers the name), but it silently leaves *this*
   * occurrence's own `A.global` record with `priv = None`, so its own
   * `T.symbol_of_global` computes a different (Public, not Private
   * idfile) hashtable key than the one actually registered -- a
   * `Not_found` at codegen time, not at load time, and easy to miss
   * until a real closure's own object happens to be private *and*
   * reached this way. *)
  let imr_operand x =
    match x with
    | Imm _ | Reg _ -> ()
    | Mem g -> gen_operand g
    | Addr e -> (match e with
        | A.Global (x, _) -> f x
        | A.Param _ | A.Local _ -> ()
      )
  in
  match i with
  | Move (_, x1, gen2) ->
      (match x1 with
      | Either.Left gen1 -> gen_operand gen1
      | Either.Right ximm1 -> A.visit_globals_ximm f ximm1
      );
      gen_operand gen2
  | Lea (g, _) -> gen_operand g
  | Call b | Jmp (b, _) | Jcc (_, b, _) -> A.visit_globals_branch_operand f b
  | Arith (_, _, imr1, gen1) -> imr_operand imr1; gen_operand gen1
  | Cmp (_, gen1, imr1) -> gen_operand gen1; imr_operand imr1
  | Test (_, _, gen1) -> gen_operand gen1
  | CmpXchg (_, _, gen1) -> gen_operand gen1
  | Lock -> ()
  | MovQToXmm (g1, _) -> gen_operand g1
  | MovQFromXmm (_, g1) -> gen_operand g1
  | PsllQXmm (_, _) -> ()
  | Shift (_, _, _, gen1) -> gen_operand gen1
  | Extend (_, gen1, _) -> gen_operand gen1
  | Unary (_, _, gen1) -> gen_operand gen1
  | MulDiv (_, _, gen1) -> gen_operand gen1
  | Imul2 (_, gen1, _) -> gen_operand gen1
  | Cwd | Cdq | Cqo -> ()
  | MovF (_, x1, x2) -> xgen_operand x1; xgen_operand x2
  | ArithF (_, _, x1, _) -> xgen_operand x1
  | CmpF (_, x1, _) -> xgen_operand x1
  | CvtIntToF (_, _, g1, _) -> gen_operand g1
  | CvtFToInt (_, _, x1, _) -> xgen_operand x1
  | CvtFPrec (_, x1, _) -> xgen_operand x1
  | XorClearF (_, _) -> ()
  | Ret | Syscall -> ()
