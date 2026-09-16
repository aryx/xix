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
open Either

open Ast_asm
open Ast_asm7

module T = Types
open Codegen

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* ARM64/AArch64 code generation.
 *
 * The 'case <n>: ...' comments below refer to goken's linkers/7l/asmout.c
 * (mirrors the Codegen{5,v,i}.ml convention).
 *
 * claude: style note -- unlike Codegen5.ml/Codegenv.ml/Codegeni.ml, which
 * build each instruction word out of several small `Bits.t` tuples (one
 * per field), the helpers here mostly compute the *whole* 32-bit word as
 * a single OCaml int via plain `lor`/`lsl`/`land`, transcribing goken's
 * own C helpers (oprrr/opirr/oaddi/olsr12u/...) close to expression-for-
 * expression, then wrap the result as a single-tuple `Bits.t` at the call
 * site. This is a deliberate choice (not an oversight): AArch64's fields
 * don't decompose as cleanly into goken's own reusable C helper functions
 * as ARM32's did, so following the C shape directly, the same way case
 * 62/63's CASE/BCASE prototype and the MCR/MRC port already did
 * successfully elsewhere in this project, is more faithful and less
 * error-prone here than re-deriving a from-scratch Bits.t decomposition.
 *
 * Scope (see docs/claude_notes/arm64_port.md for the full,
 * up-to-date status): Arith/Shift/Cmp/ArithMul have both their bare
 * (64-bit) and *W-suffixed (32-bit-view) forms; register<->memory
 * Move (Indirect and the SB-relative fast path) covers all 4 sizes
 * (B_/H_/W_/X_); the literal pool covers "MOV $bigconst,R" and
 * address-of-global; AND/ORR/EOR's own bitmask-immediate form is
 * implemented for e=64 patterns only (see bitmask_immediate_encoding's
 * own comment for the real goken bug this scoping decision is
 * responding to); the conditional-select family, TBZ/TBNZ, and the
 * X-width exclusive-monitor atomic pair are implemented too. Register-
 * immediate moves at B_/H_/W_ size ("MOVW $con,R" etc, as opposed to
 * X_'s "MOV $con,R") and register-to-register moves at those sizes
 * aren't implemented -- only X_'s "MOV Rs,Rd"/"MOV $con,Rd" are, per
 * goken's own case 24/32 (which are genuinely X_-only shapes to begin
 * with, not a narrowed subset of something wider); BIC has no
 * immediate form on real AArch64 at all.
 *)

(*****************************************************************************)
(* Types and constants *)
(*****************************************************************************)
(* claude: goken's own literal-pool mechanism (span.c/asmout.c's
 * `omovlit()`), needed for any "MOV $con,R"/"MOV $sym(SB),R" that
 * doesn't fit a direct MOVZ/MOVN -- see move_immediate_encoding's own
 * comment. Mirrors Codegen5.ml's identical `pool` type/mechanism
 * (ARM32's own literal pool); Layout7.ml does the actual splicing,
 * same shape as Layout5.ml. No `LPOOL` marker/early-flush trigger is
 * implemented on this arch yet -- only "flush at the true end of the
 * program" (see Layout7.ml), matching this port's ARM32 counterpart's
 * own current scope. *)
type pool = PoolOperand of Ast_asm.ximm

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
let error (node : 'a T.node) (s : string) =
  failwith
    (spf "%s at %s on %s" s (T.s_of_loc node.n_loc)
        (Types7.show_instr node.instr))

let w1 (x : int) : Bits.t = [(x land 0xffffffff, 0)]

(* claude: goken's `omovlit()` else-branch (the literal-pool path) --
 * "LDR (literal)", `o1 = (w<<30)|(fp<<26)|(3<<27) |
 * ((v&0x7FFFF)<<5) | dr`, w=1 for a 64-bit load (fp=0, non-float).
 * `v` is the SAME word-scaled PC-relative delta as a branch (goken's
 * `brdist(p,0,19,2)` -- literally the same call case 7/Bcc uses), so
 * this reuses `branch_delta` below by treating the pool entry's own
 * node exactly like a branch target. *)
let opldr_literal_mov = (1 lsl 30) lor (3 lsl 27)
(* claude: the float sibling (goken's own omovlit(): "case AFMOVD: fp
 * = 1; w = 1;" -- same base formula, `fp` bit ORed in). Needed since,
 * unlike ARM32/ARM64's own FArith (whose "goken's own float-immediate
 * support is dead code in the reference implementation" comment
 * explains why *that* path is skipped entirely), a plain "FMOVD
 * $1.0,F0" register-load DOES need real support here -- it's what
 * every real float constant in real 5c -S output actually compiles
 * to (found stress-testing real lib_core/libc, fmt/fltfmt.c's real
 * "FMOVD $1.0,F0"), and there's a real, working literal-pool
 * mechanism to route it through instead of goken's own broken
 * chipfloat path. See docs/claude_notes/plan_hello_libc_linking.md. *)
let opldr_literal_fmov = opldr_literal_mov lor (1 lsl 26)

(* claude: unlike ARM32 (PC = instr+8), AArch64's PC-relative fields
 * are relative to the instruction's own address with no bias --
 * same reasoning as the `branch_delta` helper further down, just
 * needed earlier here (this file's helpers are declared roughly in
 * the order goken's own asmout.c introduces the corresponding
 * concepts, and the pool mechanism comes before branches there). *)
let gload_from_pool (nsrc : 'a T.node) (rt : int) : Bits.t =
  match nsrc.T.branch with
  | None -> raise (Impossible "literal pool should be attached to node")
  | Some ndst ->
      let v = (ndst.T.real_pc - nsrc.T.real_pc) asr 2 in
      [ (opldr_literal_mov lor ((v land 0x7FFFF) lsl 5) lor rt) land 0xffffffff, 0 ]

(* claude: the float-register-destination sibling -- see
 * opldr_literal_fmov's own comment. Same PC-relative "distance to the
 * pool entry" computation, just the FP/SIMD literal-load base opcode
 * instead of the integer one. *)
let gload_from_pool_f (nsrc : 'a T.node) (rt : int) : Bits.t =
  match nsrc.T.branch with
  | None -> raise (Impossible "literal pool should be attached to node")
  | Some ndst ->
      let v = (ndst.T.real_pc - nsrc.T.real_pc) asr 2 in
      [ (opldr_literal_fmov lor ((v land 0x7FFFF) lsl 5) lor rt) land 0xffffffff, 0 ]

(*****************************************************************************)
(* Instruction encoding helpers *)
(*****************************************************************************)
(* claude: the "sf" bit (bit 31) -- 1 for the default 64-bit-register
 * forms, 0 for the *W-suffixed 32-bit-view forms (see Ast_asm7.ml's
 * arith_opcode comment: every W-suffixed row in goken's own oprrr()/
 * opirr() tables is identical to its bare counterpart with this one
 * bit cleared, nothing else differs). *)
let s64 = 1 lsl 31
let s32 = 0

(* claude: goken's `OPDP2(x) = 0<<30 | 0<<29 | 0xd6<<21 | (x)<<10`,
 * moved up here (ahead of oprrr_arith) since SDIV/UDIV's own base
 * opcodes need it too, not just case 9's shift-by-register family
 * further below. *)
let opdp2 (x : int) : int = (0xd6 lsl 21) lor (x lsl 10)

(* claude: case 1 -- register-register arith base opcodes (goken's
 * oprrr(), the AADD/ASUB/AAND/AORR/AEOR/ABIC rows and their W-suffixed
 * siblings). *)
let oprrr_arith (op : arith_opcode) : int =
  match op with
  | ADD -> s64 lor (0x0b lsl 24)
  | SUB -> s64 lor (1 lsl 30) lor (0x0b lsl 24)
  | AND_ -> s64 lor (0xA lsl 24)
  | ORR -> s64 lor (1 lsl 29) lor (0xA lsl 24)
  | EOR -> s64 lor (2 lsl 29) lor (0xA lsl 24)
  | BIC -> s64 lor (0xA lsl 24) lor (1 lsl 21)
  | ADDW -> s32 lor (0x0b lsl 24)
  | SUBW -> s32 lor (1 lsl 30) lor (0x0b lsl 24)
  | ANDW -> s32 lor (0xA lsl 24)
  | ORRW -> s32 lor (1 lsl 29) lor (0xA lsl 24)
  | EORW -> s32 lor (2 lsl 29) lor (0xA lsl 24)
  | BICW -> s32 lor (0xA lsl 24) lor (1 lsl 21)
  (* claude: real hardware SDIV/UDIV -- goken's own oprrr(), case 1's
   * plain "op Rm,[Rn,]Rd" shape (nothing pseudo-op about *division*
   * itself, only REM needs synthesis -- see Rem/orem). *)
  | SDIV -> s64 lor opdp2 3 | SDIVW -> s32 lor opdp2 3
  | UDIV -> s64 lor opdp2 2 | UDIVW -> s32 lor opdp2 2

(* claude: case 24/25 -- NEG/MVN Rn,Rd base opcodes (goken's oprrr()):
 * NEG is SUB with an implicit ZR first operand (so it shares SUB's
 * own base bits, S bit included), MVN is ORN likewise (ORR's base
 * bits plus the "invert second operand" bit 21) -- transcribed
 * directly from asmout.c's own ANEG/ANEGW/AMVN/AMVNW rows, not
 * re-derived from ADD/ORR above (kept as their own function rather
 * than reusing oprrr_arith since arith_opcode has no NEG/MVN members
 * of its own -- see Ast_asm7.ml's Neg2 comment for why). *)
let oprrr_neg2 (op : neg2_opcode) : int =
  match op with
  | NEG -> s64 lor (1 lsl 30) lor (0xB lsl 24)
  | NEGW -> s32 lor (1 lsl 30) lor (0xB lsl 24)
  | MVN -> s64 lor (1 lsl 29) lor (0xA lsl 24) lor (1 lsl 21)
  | MVNW -> s32 lor (1 lsl 29) lor (0xA lsl 24) lor (1 lsl 21)

(* claude: case 2/4 -- register-immediate ("addcon") base opcodes (goken's
 * opirr(), ADD/SUB rows and their W-suffixed siblings only -- AND/ORR/
 * EOR/BIC's immediate form (both widths) uses a different, "bitmask
 * immediate" encoding not implemented here, see this file's prelude
 * comment). *)
let opirr_addsub (op : arith_opcode) : int =
  match op with
  | ADD -> s64 lor (0x11 lsl 24)
  | SUB -> s64 lor (1 lsl 30) lor (0x11 lsl 24)
  | ADDW -> s32 lor (0x11 lsl 24)
  | SUBW -> s32 lor (1 lsl 30) lor (0x11 lsl 24)
  | AND_ | ORR | EOR ->
      raise (Impossible "AND/ORR/EOR use the bitmask-immediate path (opirr_bitmask_logical), never the addcon one")
  | ANDW | ORRW | EORW ->
      failwith "TODO: ANDW/ORRW/EORW immediate needs its own e<=32 bitcon search (N always 0), not implemented (only the bare 64-bit AND/ORR/EOR immediate is)"
  | BIC | BICW ->
      failwith "BIC has no immediate form at all on real AArch64 (confirmed absent from goken's own opirr() table, not just unimplemented here)"
  | SDIV | UDIV | SDIVW | UDIVW ->
      raise (Impossible "SDIV/UDIV have no immediate form at all on real AArch64 (register-register division only)")

(* claude: goken's oaddi() -- packs a 12-bit unsigned immediate (or,
 * shifted left by 12, up to 0xFFF000) into the "addcon" instruction
 * shape. Errors loudly (matching goken's own diag()) rather than
 * silently emitting wrong bytes for anything outside that range --
 * exactly the values case 4's "$addcon" is defined for; anything bigger
 * needs the literal pool, not implemented yet (see this file's prelude
 * comment). *)
let oaddi (node : 'a T.node) (base : int) (v : int) (r : int) (rt : int) : int =
  if v < 0 || v > 0xFFF000
  then error node "TODO: immediate out of ADD/SUB range (needs literal pool, not yet implemented)"
  else if v > 0xFFF then begin
    if v land 0xFFF <> 0
    then error node "TODO: immediate needs both direct and shifted bits (needs literal pool)"
    else base lor (1 lsl 22) lor ((v lsr 12) lsl 10) lor (r lsl 5) lor rt
  end
  else base lor (v lsl 10) lor (r lsl 5) lor rt

(* claude: case 7 -- CMP/CMN, encoded as SUBS/ADDS with an implicit ZR
 * destination (goken's own oprrr()/opirr() alias ACMP->ASUBS and
 * ACMN->AADDS onto the exact same rows, see asmout.c's oprrr/opirr
 * tables) -- kept as separate helpers here since Ast_asm7.cmp_opcode is
 * its own type, not reusing arith_opcode. *)
let oprrr_cmp (op : cmp_opcode) : int =
  match op with
  | CMP -> s64 lor (1 lsl 30) lor (1 lsl 29) lor (0x0b lsl 24) (* SUBS *)
  | CMN -> s64 lor (1 lsl 29) lor (0x0b lsl 24) (* ADDS *)
  | CMPW -> s32 lor (1 lsl 30) lor (1 lsl 29) lor (0x0b lsl 24) (* SUBSW *)
  | CMNW -> s32 lor (1 lsl 29) lor (0x0b lsl 24) (* ADDSW *)
let opirr_cmp (op : cmp_opcode) : int =
  match op with
  | CMP -> s64 lor (1 lsl 30) lor (1 lsl 29) lor (0x11 lsl 24) (* SUBS $imm *)
  | CMN -> s64 lor (1 lsl 29) lor (0x11 lsl 24) (* ADDS $imm *)
  | CMPW -> s32 lor (1 lsl 30) lor (1 lsl 29) lor (0x11 lsl 24) (* SUBSW $imm *)
  | CMNW -> s32 lor (1 lsl 29) lor (0x11 lsl 24) (* ADDSW $imm *)

(* claude: case 8 -- shift by immediate, via the bitfield-move family
 * (UBFM/SBFM for LSL/LSR/ASR, EXTR for ROR -- goken's opbfm()/opextr()).
 * `r`/`s` below are UBFM/SBFM's own "immr"/"imms" fields, already
 * pre-transformed by the caller per goken's own case 8 (e.g. LSL's
 * r=(64-v)&63, s=63-v -- not just v directly). *)
let opirr_ubfm = s64 lor (2 lsl 29) lor (0x26 lsl 23) lor (1 lsl 22)
let opirr_sbfm = s64 lor (0x26 lsl 23) lor (1 lsl 22)
let opirr_extr = s64 lor (0x27 lsl 23) lor (1 lsl 22)
(* claude: the W-suffixed 32-bit forms of the same 3 bitfield-move base
 * opcodes above -- s32 instead of s64, and the N bit (bit 22, `1 lsl
 * 22` in the 64-bit rows above) cleared instead of set, confirmed
 * against asmout.c's AUBFMW/ASBFMW/AEXTRW rows. *)
let opirr_ubfmw = s32 lor (2 lsl 29) lor (0x26 lsl 23)
let opirr_sbfmw = s32 lor (0x26 lsl 23)
let opirr_extrw = s32 lor (0x27 lsl 23)
let opbfm (base : int) (r : int) (s : int) (rf : int) (rt : int) : int =
  base lor ((r land 0x3F) lsl 16) lor ((s land 0x3F) lsl 10) lor (rf lsl 5) lor rt
let opextr (base : int) (v : int) (rn : int) (rm : int) (rt : int) : int =
  base lor (v lsl 10) lor (rn lsl 5) lor (rm lsl 16) lor rt

(* claude: case 45's SXTW/UXTW -- a fixed SBFM/UBFM(0,31,Rn,Rd), reusing
 * the exact same opirr_sbfm/opirr_ubfm bases the shift-by-immediate
 * case above already uses for ASR/LSR -- see Ast_asm7.ml's Extend
 * comment. *)
let oextend (op : extend_opcode) (rf : int) (rt : int) : int =
  match op with
  | SXTW -> opbfm opirr_sbfm 0 31 rf rt
  | UXTW -> opbfm opirr_ubfm 0 31 rf rt

(* claude: case 9 -- shift by register (LSLV/LSRV/ASRV/RORV), goken's
 * `OPDP2(x) = 0<<30 | 0<<29 | 0xd6<<21 | (x)<<10`. Same (rf<<16)|(r<<5)|rt
 * operand shape as case 1's oprrr_arith. *)
let oprrr_shift (op : shift_opcode) : int =
  match op with
  | LSL -> s64 lor opdp2 8
  | LSR -> s64 lor opdp2 9
  | ASR -> s64 lor opdp2 10
  | ROR -> s64 lor opdp2 11
  | LSLW -> s32 lor opdp2 8
  | LSRW -> s32 lor opdp2 9
  | ASRW -> s32 lor opdp2 10
  | RORW -> s32 lor opdp2 11

(* claude: case 15's simple (no from3/accumulate) MUL Rm,[Rn,]Rd, an
 * alias of MADD with Ra=ZR -- goken's `oprrr(AMUL) = S64 | 0<<29 |
 * 0x1B<<24 | 0<<21 | 0<<15`. MULW is the same row with S32 instead. *)
let oprrr_mul (op : mul_opcode) : int =
  match op with
  | MUL -> s64 lor (0x1B lsl 24)
  | MULW -> s32 lor (0x1B lsl 24)

(* claude: case 16 -- REM/REMW Rdivisor,[Rdividend,]Rdest, goken's own
 * real 2-instruction synthesis ("XremY R[,R],R -> XdivY; XmsubY" --
 * SDIV into REGTMP, X17, then MSUB back out). `sf` picks REM (s64) vs
 * REMW (s32); both instruction words share it (goken's own o2 |= o1 &
 * (1<<31) trick, replicated directly here as `sf` applied to both
 * rather than patched in after the fact -- same net bit pattern).
 * `rtmp` is REGTMP's own register number (17, Ast_asm7.rTMP). *)
(* claude: `div_op` is OPDP2's own selector -- 3 for signed (SDIV,
 * REM/REMW), 2 for unsigned (UDIV, UREM/UREMW); o2 (MSUB) has no
 * signed/unsigned distinction of its own (goken's own o2 = oprrr
 * (AMSUBW) is sign-independent, only o1's divide instruction differs
 * -- see Rem's own comment). *)
let orem (sf : int) (div_op : int) (rtmp : int) (rf : int) (r : int) (rt : int) : (int * int) =
  let o1 = sf lor opdp2 div_op lor (rf lsl 16) lor (r lsl 5) lor rtmp in
  let o2 = sf lor (0x1B lsl 24) lor (1 lsl 15) lor (rf lsl 16) lor (r lsl 10) lor (rtmp lsl 5) lor rt in
  (o1, o2)

(* claude: case 5/6 -- unconditional branch/call. `opbra(AB/ABL)` for the
 * direct-label form (imm26 field, packed by the caller), `opbrr()`
 * (goken's OPBLR macro) for the indirect-through-register form, which
 * also directly gives RET's own encoding (x=2) -- ARM64 genuinely has a
 * real RET instruction sharing this same instruction family (BR/BLR/RET
 * only differ in this 2-bit field), unlike every other arch ported so
 * far where RET is purely a synthesized virtual instr. *)
let opbra_b = 5 lsl 26 (* B: sf-like bit clear *)
let opbra_bl = s64 lor (5 lsl 26) (* BL *)
let opblr (x : int) : int = (0x6B lsl 25) lor (x lsl 21) lor (0x1F lsl 16)
let opbrr_b = opblr 0 (* BR *)
let opbrr_bl = opblr 1 (* BLR *)
let opbrr_ret = opblr 2 (* RET *)

(* claude: case 62/63 -- CASE/BCASE (switch-statement jump-table
 * dispatch), goken's real linkers/7l/asmout.c. See Ast_asm7.ml's
 * CaseJump/BCase comment for the overall mechanism (relative-offset
 * table, real 7a grammar unlike ARM32's own simplified deviation).
 *
 * `last_case_pc` mirrors goken's own `static Prog *lastcase` --
 * mutable, sequential, per-program codegen state: set by every CASE,
 * read by every following BCASE until the next CASE. Codegen7.gen's
 * own single left-to-right T.iter pass over the whole program (see
 * its own definition) is what makes this safe -- by the time a given
 * BCASE's `binary` thunk runs, the immediately-preceding CASE (if
 * any) has already updated this ref, exactly matching goken's own
 * single-pass asmout() sequencing. -1 is deliberately not a valid PC,
 * so a BCASE with no preceding CASE fails loudly (goken's own
 * "missing CASE" diag()) rather than silently computing a bogus
 * offset. *)
let last_case_pc = ref (-1)

(* claude: real ADR (p=0 -- ADRP, p=1, is never needed here) --
 * goken's own `#define ADR(p,o,rt)`. immlo = the low 2 bits of the
 * byte offset (bits[30:29]), immhi = the rest, shifted right by 2
 * (bits[23:5]) -- only ever called here with imm=16 (immlo=0), but
 * implemented in general for clarity rather than hardcoding that. *)
let oadr (imm : int) (rt : int) : int =
  (0x10 lsl 24) lor ((imm land 3) lsl 29) lor (((imm asr 2) land 0x7FFFF) lsl 5) lor rt
(* claude: "movw Rt[Rv<<2],REGTMP" -- LDRSW (load register, signed
 * word, scaled register offset) reading a 32-bit table entry at
 * Rt+Rv*4 into REGTMP, transcribed directly from asmout.c's own case
 * 62 bit pattern, not re-derived from the general LDSTX/extended-
 * register addressing families elsewhere in this file (this one
 * exact shape was never needed anywhere else). *)
let ocase_load (rv : int) (rt : int) (rtmp : int) : int =
  (2 lsl 30) lor (7 lsl 27) lor (2 lsl 22) lor (1 lsl 21) lor (3 lsl 13)
    lor (1 lsl 12) lor (2 lsl 10) lor (rv lsl 16) lor (rt lsl 5) lor rtmp

(* claude: case 7 (branch variant) -- BEQ/BNE/...; goken's OPBcc(x) =
 * 0x2A<<25 | (x&15), the condition packed into a distinct base opcode
 * from the branch's own PC-relative imm19 field (ORed in by the caller
 * via `<<5`, see branch_delta_19 below). *)
let opbcc (cond : int) : int = (0x2A lsl 25) lor (cond land 0xF)
let int_of_condition (c : condition) : int =
  match c with
  | EQ -> 0x0 | NE -> 0x1
  | GE U -> 0x2 | LT U -> 0x3
  | MI -> 0x4 | PL -> 0x5
  | VS -> 0x6 | VC -> 0x7
  | GT U -> 0x8 | LE U -> 0x9
  | GE S -> 0xa | LT S -> 0xb
  | GT S -> 0xc | LE S -> 0xd
  | AL -> 0xe

(* claude: case 8's own (distinct) LTYPE -- CBZ/CBNZ Rt,label. Goken's
 * `0x1A<<25 | (nz)<<24`, the imm19 branch field ORed in at bit5 by the
 * caller, same as opbcc above. *)
let opcbz (nonzero : bool) : int = (0x1A lsl 25) lor ((if nonzero then 1 else 0) lsl 24)

(* claude: case 10 -- SVC (goken's opimm(), the ASVC row only; BRK/HVC/
 * HLT/etc share this LTYPE but aren't wired -- see Ast_asm7.ml). *)
let opimm_svc = (0xD4 lsl 24) lor 1

(* claude: case 20/21 -- MOV load/store, scaled-12-bit-unsigned-offset
 * form only (goken's `v >= 0` branch of case 20/21; the `v < 0`
 * "unscaled 9-bit signed" branch, and the pre/post-increment "!" forms
 * of case 22/23, aren't implemented yet). goken's `LDSTR12U(sz,v,opc)
 * = sz<<30 | 7<<27 | v<<26 | 1<<24 | opc<<22` with (sz=3,v=0,opc=1)
 * for AMOV's *load*; store is the same opcode with opc's bit cleared
 * (goken's `LD2STR`, `o & ~(3<<22)`). *)
let ldstr12u (sz : int) (v : int) (opc : int) : int =
  (sz lsl 30) lor (7 lsl 27) lor (v lsl 26) lor (1 lsl 24) lor (opc lsl 22)
let opldr12_mov = ldstr12u 3 0 1
let opstr12_mov = opldr12_mov land (lnot (3 lsl 22))
let olsr12u (node : 'a T.node) (base : int) (v : int) (b : int) (r : int) : int =
  if v < 0 || v >= (1 lsl 12)
  then error node "TODO: offset out of 12-bit scaled range (needs literal pool / unscaled form, not yet implemented)"
  else base lor ((v land 0xFFF) lsl 10) lor (b lsl 5) lor r

(* claude: the byte/halfword/32-bit-view sized siblings of opldr12_mov/
 * opstr12_mov above -- goken's own opldr12()/opstr12() table (`(sz,opc)`
 * per mnemonic: AMOVB->(0,2), AMOVBU->(0,1), AMOVH->(1,2), AMOVHU->(1,1),
 * AMOVW->(2,2) [sign-extend into the 64-bit dest], AMOVWU->(2,1)
 * [zero-extend]; `opc` only ever matters for *loads* -- stores always
 * clear it via LD2STR, same as opstr12_mov above, since a narrow store
 * has no sign/zero-extension to speak of). The scaled-12-bit
 * immediate's own scale factor is the size in bytes (1/2/4/8 for
 * B/H/W/X), goken's `offsetshift()` -- same "offset divided by the
 * access size, must divide evenly" shape already used for the X_-only
 * helpers above, just generalized to the other 3 sizes here. *)
(* claude: extended to a (sz,v,opc) triple to also cover FMOVS/FMOVD's
 * memory form -- goken's own opldr12(AFMOVS)=LDSTR12U(2,1,1),
 * opldr12(AFMOVD)=LDSTR12U(3,1,1): same LDSTR12U shape as the integer
 * sizes, just the "V" bit (float-vs-integer register file) set to 1
 * instead of 0. SCVTF_*/FCVTZS_* are never a memory-access shape in
 * goken (register-to-register only, case 29, see Move's FCvt-style
 * arms in `rules` below) so they error loudly here rather than
 * silently producing a bogus memory encoding. *)
let ldstr12u_size_v_opc (ms : move_size) : int * int * int =
  match ms with
  | B_ A.S -> 0, 0, 2 | B_ A.U -> 0, 0, 1
  | H_ A.S -> 1, 0, 2 | H_ A.U -> 1, 0, 1
  | W_ A.S -> 2, 0, 2 | W_ A.U -> 2, 0, 1
  | X_ -> 3, 0, 1
  | FS_ -> 2, 1, 1
  | FD_ -> 3, 1, 1
  | SCVTF_S | SCVTF_D | FCVTZS_S | FCVTZS_D | UCVTF_WD | FCVTZU_WD | SCVTF_WD | FCVTZS_WD ->
      failwith "Codegen7: SCVTF/FCVTZS/UCVTF/FCVTZU is register-to-register only, not a memory shape"
let scale_shift_of_size (ms : move_size) : int =
  match ms with
  | B_ _ -> 0 | H_ _ -> 1 | W_ _ | FS_ -> 2 | X_ | FD_ -> 3
  | SCVTF_S | SCVTF_D | FCVTZS_S | FCVTZS_D | UCVTF_WD | FCVTZU_WD | SCVTF_WD | FCVTZS_WD ->
      failwith "Codegen7: SCVTF/FCVTZS/UCVTF/FCVTZU is register-to-register only, not a memory shape"
let opldr12_sized (ms : move_size) : int =
  let sz, v, opc = ldstr12u_size_v_opc ms in
  ldstr12u sz v opc
let opstr12_sized (ms : move_size) : int =
  (opldr12_sized ms) land (lnot (3 lsl 22))

(* claude: the unscaled, signed-9-bit-immediate sibling of ldstr12u
 * above (goken's own `#define LDSTR9S(sz,v,opc)` -- identical bit
 * layout to LDSTR12U except bit 24 is 0 instead of 1, so it reuses
 * the exact same (sz,v,opc) table, ldstr12u_size_v_opc). Used as a
 * fallback (goken's own case 20/21: "if(v<0) unscaled else scaled")
 * whenever an offset doesn't divide evenly by the access size and so
 * can't use the scaled-12-bit form at all -- confirmed needed by a
 * real lib_core/libc closure (arch/arm64/rt0.s's real "MOV
 * R1,_mainargv+0(SB)", where _mainargv's data-segment offset isn't
 * 8-byte-aligned once merged with 34 other objects -- no hand-written
 * arm64_diff/ fixture ever exercised an odd SB-relative offset, only
 * a real multi-object link does). Range is a real hardware constraint
 * (9-bit signed immediate, -256..255), not a not-yet-implemented gap
 * like the scaled form's own "needs literal pool" TODO just below. *)
let ldstr9s (sz : int) (v : int) (opc : int) : int =
  (sz lsl 30) lor (7 lsl 27) lor (v lsl 26) lor (opc lsl 22)
let olsr9s (node : 'a T.node) (base : int) (v : int) (b : int) (r : int) : int =
  if v < -256 || v > 255
  then error node "TODO: unaligned SB-relative offset also out of unscaled 9-bit range (needs literal pool, not yet implemented)"
  else base lor ((v land 0x1FF) lsl 12) lor (b lsl 5) lor r
let opldr9_sized (ms : move_size) : int =
  let sz, v, opc = ldstr12u_size_v_opc ms in
  ldstr9s sz v opc
let opstr9_sized (ms : move_size) : int =
  (opldr9_sized ms) land (lnot (3 lsl 22))

(* claude: case 22/23 -- pre/post-index writeback load/store ("MOV
 * Rt,-16(Rbase)!" / "MOV Rt,(Rbase)16!"), goken's `opldrpp()`
 * (AMOV row: `3<<30 | 7<<27 | 0<<26 | 0<<24 | 1<<22`, LD2STR clears
 * the opc field for the store direction) plus a `v<<12` signed 9-bit
 * offset and a mode field at bits[11:10] (goken: `1<<10` for
 * post-index, `3<<10` for pre-index). Needed for Rewrite7.ml's
 * RETURN-expansion link-register save/restore. *)
let opldrpp_mov = (3 lsl 30) lor (7 lsl 27) lor (1 lsl 22)
let opstrpp_mov = opldrpp_mov land (lnot (3 lsl 22))
let ldrstr_pp (node : 'a T.node) (base : int) (is_post : bool) (v : int) (b : int) (r : int) : int =
  if v < -256 || v > 255
  then error node "TODO: pre/post-index offset out of 9-bit signed range"
  else
    let mode_bits = if is_post then 1 else 3 in
    base lor (mode_bits lsl 10) lor ((v land 0x1FF) lsl 12) lor (b lsl 5) lor r

(* claude: case 32 -- "MOV $con,R -> movz/movn". A plain integer literal
 * for a MOV pseudo-op goes through goken's full `aclass()`/`cmp()`
 * constant-classification chain (span.c), NOT just a direct movcon()
 * check as this file's first attempt assumed (caught the hard way: "MOV
 * $42,R0" assembles to a real MOVZ, but "MOV $7,R0" -- a value that
 * ALSO trivially fits one 16-bit lane -- assembles to a literal-pool
 * LDR instead, confirmed directly against goken; the difference is
 * that 7 is also a valid ARM64 "logical immediate" bit pattern
 * (`isbitcon`, a contiguous run of 1-bits, possibly rotated/replicated)
 * while 42 isn't, and aclass() checks that classification *before* ever
 * considering movcon() -- see isbitcon's own comment below). The real
 * priority chain (span.c's aclass() D_CONST case, cross-referenced with
 * cmp()'s C_MOVCON compatibility list, which only accepts C_ZCON/
 * C_ADDCON0, not the more general C_ADDCON/C_ABCON):
 *  1. v = 0 -> MOVZ #0 (trivially lane 0).
 *  2. isaddcon(v) [fits ADD/SUB's own "$addcon" 12-bit-optionally-
 *     shifted-by-12 immediate shape -- same range check as oaddi above]:
 *     - isbitcon(v) -> needs the pool (C_ABCON, not movcon-compatible).
 *     - v <= 0xFFF -> MOVZ, lane 0 (C_ADDCON0, the one addcon subclass
 *       cmp() does accept for C_MOVCON).
 *     - else (the "shifted by 12" ADDCON case) -> needs the pool
 *       (C_ADDCON itself isn't movcon-compatible either).
 *  3. else, movcon(v) -- v fits entirely within one 16-bit lane, the
 *     other 3 all zero -> MOVZ at that lane.
 *  4. else, movcon(lnot v) -- v's bitwise complement fits one lane
 *     (the other 3 lanes of v are all ones) -> MOVN (loads the
 *     complement) at that lane.
 *  5. else -> needs the literal pool (case 12), not implemented yet.
 *)
let isaddcon (v : int) : bool =
  v >= 0 && (if v land 0xFFF = 0 then v asr 12 else v) <= 0xFFF

(* claude: ARM64's "logical immediate" bitmask-pattern test (goken's
 * `isbitcon`/`findmask`, span.c) -- is `v` some rotation of a
 * contiguous run of 1-bits, replicated at some power-of-two element
 * size (2/4/8/16/32/64) that evenly divides the register width? Ported
 * as a direct search rather than goken's closed-form `findmask` (which
 * relies on x86-style bit-scan intrinsics not worth reproducing here)
 * -- same answer, just less clever. The e=64 (no replication) case is
 * checked directly as "v equals some contiguous run of 1..63 ones at
 * some bit position" rather than via 64-bit rotation, since OCaml's
 * native int is 63 bits and every constant this port deals with is a
 * small nonnegative value with no bits anywhere near bit 63 -- a
 * wrapping rotation could only ever matter for a value with high bits
 * set, which doesn't arise here. Verified directly against goken for
 * several values (7/15/3/1/0xff -> bitcon, pool; 42/100/5 -> not
 * bitcon, MOVZ) before trusting this. *)
let isbitcon (v : int) : bool =
  let is_contig_run_e64 (v : int) : bool =
    if v <= 0 then false
    else
      let rec try_n n =
        if n >= 63 then false
        else
          let pat = (1 lsl n) - 1 in
          let rec try_k k =
            if k > 62 - n then false
            else if pat lsl k = v then true
            else try_k (k + 1)
          in
          if try_k 0 then true else try_n (n + 1)
      in try_n 1
  in
  let rotate_right_e (p : int) (e : int) (r : int) : int =
    let mask = (1 lsl e) - 1 in
    let p = p land mask in
    if r = 0 then p else ((p lsr r) lor (p lsl (e - r))) land mask
  in
  let is_contig_ones (p : int) (e : int) : bool =
    let mask = (1 lsl e) - 1 in
    let p = p land mask in
    if p = 0 || p = mask then false
    else
      let rec try_r r =
        if r >= e then false
        else if rotate_right_e p e r land (rotate_right_e p e r + 1) = 0
        then true
        else try_r (r + 1)
      in try_r 0
  in
  let rec try_e e =
    if e >= 64 then is_contig_run_e64 v
    else
      let low = v land ((1 lsl e) - 1) in
      let reps = 64 / e in
      let full =
        let rec build acc n = if n = 0 then acc else build ((acc lsl e) lor low) (n - 1) in
        build 0 reps
      in
      let checkbits = min 62 (e * reps) in
      let vmask = (1 lsl checkbits) - 1 in
      if full land vmask = v land vmask && is_contig_ones low e
      then true
      else try_e (e * 2)
  in try_e 2

(* claude: AND/ORR/EOR's own *immediate* form (case 53, a genuinely
 * different "bitmask immediate" encoding from ADD/SUB's plain
 * "addcon" one) -- scoped to ONLY the e=64 case (a single contiguous
 * run of 1-bits, any rotation, NOT further replicated at any smaller
 * power-of-two element size), never a smaller replicated element,
 * unlike `isbitcon` above (which does search smaller e, for the
 * unrelated MOV-immediate classification question of "does this
 * value need the literal pool"). This is a deliberate, empirically-
 * forced scope decision, not laziness: direct testing against real
 * goken turned up a genuine bug in its own bitmask-immediate
 * assembler for sub-64-bit element sizes -- "AND $0x0202020202020202,
 * R1,R2" (a clean e=8 pattern, one bit set per byte) assembles with
 * goken's real 7a/7l to bytes that decode back to 0x0200000002000000,
 * a DIFFERENT value than requested (confirmed by manually decoding
 * the raw instruction word's N/immr/imms fields against the standard
 * ARM64 algorithm, not just trusting objdump's summary line) --
 * likewise "AND $0x0101010101010101,R1,R2" (e=8) round-trips as only
 * 0x0000000100000001 (e=32). This smells like a real bug in goken's
 * own findmask64()/maxstr1() for element sizes below 32/64, not
 * something worth differentially testing against (there's no
 * confidence the "bug" is even stable/well-defined across inputs) --
 * far more brittle than the "big vs small constant" MOV-immediate
 * surprise this file's `move_immediate_encoding` already documents.
 * Values needing e<64 are therefore simply not classified as bitcon
 * here at all; they fall through to `bitmask_immediate_encoding`
 * returning `None`, and the caller in `rules` errors loudly rather
 * than emitting anything (matching this port's own consistent
 * "loud error over silently-wrong bytes" policy) -- always at least
 * as correct as replicating goken's own confirmed-buggy direct
 * encoding.
 *
 * The (run-length, left-shift) pair search below reuses the exact
 * same shape as `isbitcon`'s own `is_contig_run_e64` helper (already
 * proven correct via extensive testing), just returning the
 * parameters instead of a bare bool. immr/imms/N were derived AND
 * verified empirically against goken (not just read off asmout.c's
 * C source, given the elevated risk already found in this family):
 * for v=1 (n=1,k=0) goken emits N=1,immr=0,imms=0; v=3 (n=2,k=0)
 * gives imms=1; v=7 (n=3,k=0) gives imms=2; v=0x1FE=0xFF<<1 (n=8,k=1,
 * a genuinely ROTATED pattern) gives immr=63,imms=7 -- confirming
 * imms=n-1 and immr=(64-k) mod 64 (i.e. the ARM64 decode's own
 * "pattern = ROR(ones(n), immr)" convention: rotating the canonical
 * ones-from-bit-0 pattern RIGHT by immr must reproduce v, so a
 * pattern that's really a LEFT-shift-by-k of the canonical form needs
 * immr = 64-k, not k itself). *)
let bitcon64_params (v : int) : (int * int) option (* (n, k) *) =
  if v <= 0 then None
  else
    let rec try_n n =
      if n >= 63 then None
      else
        let pat = (1 lsl n) - 1 in
        let rec try_k k =
          if k > 62 - n then None
          else if pat lsl k = v then Some (n, k)
          else try_k (k + 1)
        in
        match try_k 0 with
        | Some r -> Some r
        | None -> try_n (n + 1)
    in try_n 1

(* claude: case 53's own base opcodes (goken's opirr(), the AAND/AORR/
 * AEOR rows only -- BIC genuinely has no immediate form on real
 * AArch64 at all, confirmed absent from opirr()'s table, not just
 * unimplemented here, see opirr_addsub's own comment). Only the
 * bare (64-bit) forms: the *W-suffixed 32-bit-view immediate form
 * needs its own, narrower bitcon-at-e<=32 search (N is always 0
 * there, never 1) that this port hasn't verified at all yet -- not
 * wired, see bitmask_immediate_encoding's own dispatch below. *)
let opirr_bitmask_logical (op : arith_opcode) : int =
  match op with
  | AND_ -> s64 lor (0x24 lsl 23)
  | ORR -> s64 lor (1 lsl 29) lor (0x24 lsl 23)
  | EOR -> s64 lor (2 lsl 29) lor (0x24 lsl 23)
  | ADD | SUB | BIC | ADDW | SUBW | BICW | ANDW | ORRW | EORW
  | SDIV | UDIV | SDIVW | UDIVW ->
      raise (Impossible "opirr_bitmask_logical: not a supported bitmask-immediate opcode")

let bitmask_immediate_encoding (op : arith_opcode) (v : int) (r : int) (rt : int) : int option =
  let* (n, k) = bitcon64_params v in
  let imms = n - 1 in
  let immr = (64 - k) mod 64 in
  Some (opirr_bitmask_logical op lor (1 lsl 22) lor (immr lsl 16)
        lor (imms lsl 10) lor (r lsl 5) lor rt)

let movcon (v : int) : int option =
  let rec aux s =
    if s >= 4 then None
    else if v land (lnot (0xFFFF lsl (s * 16))) = 0 then Some s
    else aux (s + 1)
  in aux 0
let opirr_movz = s64 lor (2 lsl 29) lor (0x25 lsl 23)
let opirr_movn = s64 lor (0x25 lsl 23)
(* claude: the 32-bit-view siblings (goken's own AMOVZW/AMOVNW rows --
 * s32 instead of s64, nothing else differs, same convention as every
 * other *W-suffixed pair in this file). *)
let opirr_movzw = s32 lor (2 lsl 29) lor (0x25 lsl 23)
let opirr_movnw = s32 lor (0x25 lsl 23)

(* claude: the full classification chain (see the block comment above)
 * -- `None` means "needs the literal pool, not implemented yet". The
 * result still needs `rt` (bits[4:0]) ORed in by the caller.
 * Generalized to `is_w` (32-bit MOVW/MOVN vs 64-bit MOV/MOVN) since a
 * real closure needs "MOVW $4,R5" too, not just the 64-bit form --
 * goken's own case 32 threads a width cap (`r`, 32 or 64) through
 * `movcon`'s own found shift position the same way, confirmed against
 * asmout.c directly rather than assumed from the 64-bit form alone. *)
let move_immediate_encoding_sized (is_w : bool) (v : int) : int option =
  let movz, movn, max_s =
    if is_w then opirr_movzw, opirr_movnw, 2 else opirr_movz, opirr_movn, 4 in
  if v = 0 then Some movz
  else if isaddcon v then
    (if isbitcon v || v > 0xFFF then None
     else Some (movz lor (v lsl 5)))
  else
    match movcon v with
    | Some s when s < max_s -> Some (movz lor (((v asr (s*16)) land 0xFFFF) lsl 5) lor (s lsl 21))
    | _ ->
        (match movcon (lnot v) with
        | Some s when s < max_s ->
            let d = lnot v in
            Some (movn lor (((d asr (s*16)) land 0xFFFF) lsl 5) lor (s lsl 21))
        | _ -> None)
let move_immediate_encoding (v : int) : int option = move_immediate_encoding_sized false v

(* claude: case 24 -- register-to-register "MOV Rs,Rd", a real AArch64
 * pseudo-op for either ORR Rs,ZR,Rd (the common case) or ADD $0,Rs,Rd
 * (used instead whenever SP is involved on either side, since ORR can't
 * reference SP as a source/dest register in the real ISA -- goken's own
 * `s = rf==REGSP || rt==REGSP` check). *)
let gmov_reg_reg (rf_i : int) (rt_i : int) : int =
  if rf_i = 31 || rt_i = 31 (* RSP/ZR share register 31 -- see Ast_asm7.ml's
                             * prelude comment; this reuses the same
                             * SP-aware branch goken's case 24 takes for
                             * ZR too, which is harmless since ORR
                             * Rzr,ZR,Rd and ADD $0,Rzr,Rd compute the
                             * same result either way *)
  then (opirr_addsub ADD) lor (rf_i lsl 5) lor rt_i (* ADD $0,Rf,Rt *)
  else (oprrr_arith ORR) lor (rf_i lsl 16) lor (31 lsl 5) lor rt_i (* ORR Rf,ZR,Rt *)

(* claude: branch offset -- unlike ARM32 (PC = current instr + 8, a
 * classic 3-stage-pipeline quirk) AArch64 PC-relative fields are always
 * relative to the branch/CBZ/Bcc instruction's *own* address, no bias at
 * all. `real_pc` is already absolute (Layout7.ml's `pc := ref init_text`)
 * so no separate +INITTEXT term is needed either, same reasoning as
 * Codegeni.ml's (RISC-V) branch_delta. *)
(* claude: caught a real bug the hard way (byte-diff against goken):
 * AArch64's PC-relative branch immediates are *word*-scaled (goken's
 * own `brdist(..., shift=2)` -- every field here is `(target-pc)>>2`,
 * not the raw byte delta), since every instruction is 4-byte aligned.
 * This first version forgot the `>>2` entirely, sending every non-
 * adjacent branch target 4x too far. All real_pc values are absolute
 * and always a multiple of 4, so a plain arithmetic shift is exact and
 * sign-preserving for backward branches. *)
let branch_delta (node : 'a T.node) : int =
  match node.branch with
  | None -> raise (Impossible "resolving should have set the branch field")
  | Some ndst -> (ndst.real_pc - node.real_pc) asr 2

(* claude: goken's float-arith/compare/register-move/conversion base
 * opcodes (asmout.c's `#define FPOP2S/FPOP1S/FPCMP/FPCVTI` macros,
 * transcribed directly, `m`/`s` always 0 here -- ARMv8.2 half-
 * precision and different rounding modes aren't wired). `type_`
 * selects single (0) vs double (1) precision -- see this file's
 * fp_arith_opcode/move_size comments for why S/D are sibling
 * constructors rather than a separate width flag, same convention as
 * the *W-suffixed integer forms. *)
let fpop2s (type_ : int) (op : int) : int =
  (0x1E lsl 24) lor (type_ lsl 22) lor (1 lsl 21) lor (op lsl 12) lor (2 lsl 10)
(* claude: case 54 -- dyadic float arith (FADD/FSUB/FMUL/FDIV, both
 * precisions); goken's own float-immediate operand support is dead
 * code in the reference implementation (see Ast_asm7.ml's FArith
 * comment), so only the register-register form is ported. *)
let oprrr_farith (op : fp_arith_opcode) : int =
  match op with
  | FADDS -> fpop2s 0 2 | FADDD -> fpop2s 1 2
  | FSUBS -> fpop2s 0 3 | FSUBD -> fpop2s 1 3
  | FMULS -> fpop2s 0 0 | FMULD -> fpop2s 1 0
  | FDIVS -> fpop2s 0 1 | FDIVD -> fpop2s 1 1

let fpcmp (type_ : int) : int =
  (0x1E lsl 24) lor (type_ lsl 22) lor (1 lsl 21) lor (8 lsl 10)
(* claude: case 56 -- FCMPS/FCMPD, no destination register (compares
 * Fm,Fn and sets the condition flags, same as the integer Cmp's
 * implicit-ZR-destination shape). *)
let oprrr_fcmp (op : fp_cmp_opcode) : int =
  match op with FCMPS -> fpcmp 0 | FCMPD -> fpcmp 1

let fpop1s (type_ : int) (op : int) : int =
  (0x1E lsl 24) lor (type_ lsl 22) lor (1 lsl 21) lor (op lsl 15) lor (0x10 lsl 10)
(* claude: case 54's own "monadic" branch (goken detects a FPOP1S-
 * shaped opcode -- bit 11 clear, unlike FPOP2S's bit 11 set -- and
 * reuses the *same* case for both dyadic and monadic float ops) --
 * FMOVS/FMOVD's register-to-register form (op=0) lands here since
 * it's dispatched as a Move (move_size FS_/FD_), not FArith; goken's
 * own FABSS/FABSD/FNEGS/FNEGD/FSQRTS/FSQRTD/FCVTSD/FCVTDS share this
 * same FPOP1S shape (different `op` values) but aren't wired as their
 * own mnemonics in this port yet. *)
let oprrr_fmovreg (ms : move_size) : int =
  match ms with
  | FS_ -> fpop1s 0 0 | FD_ -> fpop1s 1 0
  | B_ _ | H_ _ | W_ _ | X_ | SCVTF_S | SCVTF_D | FCVTZS_S | FCVTZS_D | UCVTF_WD | FCVTZU_WD | SCVTF_WD | FCVTZS_WD ->
      raise (Impossible "oprrr_fmovreg: not FS_/FD_")

(* claude: case 29 -- SCVTF*/FCVTZS*/UCVTF* (int<->float conversion),
 * goken's `#define FPCVTI(sf,s,type,rmode,op)`. `sf` selects whether
 * the *integer* side is a 64-bit X register (1) or 32-bit W register
 * (0, UCVTF_WD's own form -- goken's other *W variants, e.g.
 * SCVTFWD/FCVTZSDW, aren't ported); `type_` selects the *float*
 * side's precision (0=S,1=D) regardless of which side is the
 * conversion's source vs destination; `rmode`/`op` distinguish
 * int->float (0, 2=signed/3=unsigned) from float->int-truncating
 * (3, 0=signed/1=unsigned) -- values transcribed directly from
 * asmout.c's ASCVTFD/ASCVTFS/AFCVTZSD/AFCVTZSS/AUCVTFWD rows, not
 * re-derived. *)
let fpcvti (sf : int) (type_ : int) (rmode : int) (op : int) : int =
  (sf lsl 31) lor (0x1E lsl 24) lor (type_ lsl 22) lor (1 lsl 21)
    lor (rmode lsl 19) lor (op lsl 16)
let oprrr_fcvt (ms : move_size) : int =
  match ms with
  | SCVTF_S -> fpcvti 1 0 0 2 | SCVTF_D -> fpcvti 1 1 0 2
  | FCVTZS_S -> fpcvti 1 0 3 0 | FCVTZS_D -> fpcvti 1 1 3 0
  (* claude: UCVTFWD -- unsigned 32-bit (W reg, sf=0) int -> double
   * (type_=1), convert-to-float direction (rmode=0), unsigned (op=3).
   * See Ast_asm7.ml's UCVTF_WD comment for the wider, not-yet-needed
   * family this generalizes from. *)
  | UCVTF_WD -> fpcvti 0 1 0 3
  | FCVTZU_WD -> fpcvti 0 1 3 1
  | SCVTF_WD -> fpcvti 0 1 0 2
  | FCVTZS_WD -> fpcvti 0 1 3 0
  | B_ _ | H_ _ | W_ _ | X_ | FS_ | FD_ ->
      raise (Impossible "oprrr_fcvt: not SCVTF_*/FCVTZS_*/UCVTF_WD")

(* claude: case 51 -- DMB/DSB/ISB $imm, goken's `#define
 * SYSOP(l,op0,op1,crn,crm,op2,rt)` with the user's immediate ORed
 * directly into the crm field's own bit position (`o1 |=
 * (offset&0xF)<<8`, transcribed as-is -- see Ast_asm7.ml's Barrier
 * comment for why bare NOP/HINT and SYS/SYSL/MRS/MSR aren't wired
 * here). *)
let sysop (l : int) (op0 : int) (op1 : int) (crn : int) (crm : int)
    (op2 : int) (rt : int) : int =
  (0x354 lsl 22) lor (l lsl 21) lor (op0 lsl 19) lor (op1 lsl 16)
    lor (crn lsl 12) lor (crm lsl 8) lor (op2 lsl 5) lor rt
let opirr_barrier (op : barrier_opcode) : int =
  match op with
  | DSB_ -> sysop 0 0 3 3 0 4 0x1F
  | DMB_ -> sysop 0 0 3 3 0 5 0x1F
  | ISB_ -> sysop 0 0 3 3 0 6 0x1F

(* claude: case 40 -- TBZ/TBNZ $bit,Rt,label. goken's own base opcode
 * (`opirr()`, `0x36<<24`/`0x37<<24`) plus a split bit-number field
 * (bit 5 of the tested-bit-number at instruction bit 31, the low 5
 * bits at instruction bits[23:19] -- goken's `((v&0x20)<<(31-5)) |
 * ((v&0x1F)<<19)`, transcribed directly) and a 14-bit word-scaled
 * PC-relative branch field at bits[18:5] (same `branch_delta`
 * mechanism as every other branch here, just a narrower field and
 * shifted by 5 instead of the caller ORing at bit 0). *)
let opirr_tbz (nonzero : bool) : int = if nonzero then 0x37 lsl 24 else 0x36 lsl 24
let tbz_bitfield (bit : int) : int =
  ((bit land 0x20) lsl (31 - 5)) lor ((bit land 0x1F) lsl 19)

(* claude: case 18 -- CSEL/CSINC/CSINV/CSNEG/CSET/CSETM (goken's
 * oprrr(), transcribed directly; see CondSel/CondSet's own AST
 * comments for why CINC/CINV/CNEG share CSINC/CSINV/CSNEG's row, and
 * why CSET/CSETM's own rows are byte-identical to CSINC/CSINV's --
 * they're genuinely the same base opcode, confirmed reading asmout.c,
 * not a coincidence). *)
let oprrr_condsel (op : cond_sel_opcode) : int =
  let base = 0xD4 lsl 21 in
  match op with
  | CSEL -> s64 lor base | CSELW -> s32 lor base
  | CSINC -> s64 lor base lor (1 lsl 10) | CSINCW -> s32 lor base lor (1 lsl 10)
  | CSINV -> s64 lor (1 lsl 30) lor base | CSINVW -> s32 lor (1 lsl 30) lor base
  | CSNEG -> s64 lor (1 lsl 30) lor base lor (1 lsl 10)
  | CSNEGW -> s32 lor (1 lsl 30) lor base lor (1 lsl 10)
let oprrr_condset (op : cond_set_opcode) : int =
  let base = 0xD4 lsl 21 in
  match op with
  | CSET -> s64 lor base lor (1 lsl 10) | CSETW -> s32 lor base lor (1 lsl 10)
  | CSETM -> s64 lor (1 lsl 30) lor base | CSETMW -> s32 lor (1 lsl 30) lor base

(* claude: case 58/59 -- the exclusive-monitor atomic pair (goken's
 * `#define LDSTX(sz,o2,l,o1,o0)`, and opload()/opstore()'s own
 * ALDXR/ALDAXR/ASTXR/ASTLXR rows, transcribed directly). Scoped to
 * X-width (sz=3) only -- see LoadExcl/StoreExcl's own AST comments
 * for what's deliberately out of scope and why this family got extra
 * empirical verification (operand order confirmed by disassembling a
 * concrete instruction against real goken) rather than trusting the
 * C source alone, given its documented bug history. *)
let ldstx (sz : int) (o2 : int) (l : int) (o1 : int) (o0 : int) : int =
  (sz lsl 30) lor (0x8 lsl 24) lor (o2 lsl 23) lor (l lsl 22) lor (o1 lsl 21) lor (o0 lsl 15)
let opload_excl (acquire : bool) : int =
  (if acquire then ldstx 3 0 1 0 1 else ldstx 3 0 1 0 0) lor (0x1F lsl 10)
let opstore_excl (release : bool) : int =
  (if release then ldstx 3 0 0 0 1 else ldstx 3 0 0 0 0) lor (0x1F lsl 10)

(* claude: the "huge offset" fallback for an SB-relative access whose
 * offset fits neither the scaled-12-bit nor the unscaled-9-bit form
 * (goken's own case 47/48, "Hugestxr"/"Hugeldxr" -- a genuinely new
 * "olsxrr" extended-register addressing family this port doesn't
 * implement, needed once real closures grow past a single small
 * hand-written fixture's own tiny data segment). Simpler xix-only
 * equivalent instead: materialize the symbol's own ABSOLUTE address
 * into REGTMP via the exact same literal-pool mechanism Address
 * (Global) already uses (gload_from_pool), then a plain zero-offset
 * STR/LDR through REGTMP -- semantically equivalent, not byte-
 * identical to goken's own register-offset scheme. One function
 * covers both GReg and GFReg callers: opstr12_sized/opldr12_sized
 * already dispatch correctly on `ms` (the "V" float/integer register-
 * file bit) either way, and `r` is just a plain register number by
 * the time it gets here regardless of which kind it came from. Found
 * stress-testing real lib_core/libc (arch/arm64/rt0.s's real "MOV
 * R1,_mainargv+0(SB)", once the merged closure's data segment grew
 * past +-256 bytes), see
 * docs/claude_notes/plan_hello_libc_linking.md. *)
let gsbrel_huge (node : 'a T.node) (ms : move_size) (global : A.global) (goffset : int)
    (rtmp : int) (r : int) (is_store : bool) : pool Codegen.action =
  { size = 8; x = Some (PoolOperand (Ast_asm.Address (Global (global, goffset))));
    binary = (fun () ->
      let access =
        if is_store
        then olsr12u node (opstr12_sized ms) 0 rtmp r
        else olsr12u node (opldr12_sized ms) 0 rtmp r
      in
      [ gload_from_pool node rtmp; w1 access ]
    )}

(* claude: the same "huge offset" idea as gsbrel_huge just above, for a
 * plain register-relative Indirect access (base register + offset, no
 * SB/global involved) whose offset fits neither the scaled-12-bit nor
 * the unscaled-9-bit form. xix-only equivalent: materialize the
 * offset itself into REGTMP (a direct MOVZW/MOVNW when it fits one
 * 16-bit lane, else the literal pool -- same move_immediate_encoding_
 * sized/gload_from_pool choice already used everywhere else in this
 * file), add it onto Rbase (plain register-register ADD, case 1) to
 * form the real address in REGTMP, then a plain zero-offset STR/LDR
 * through REGTMP -- 3 instructions either way, so `size` is always 12
 * regardless of which offset-materialization branch is taken. Found
 * stress-testing real lib_core/libc (fmt/dofmt.c's real "MOVW
 * -8(R3),R3"), see docs/claude_notes/plan_hello_libc_linking.md. *)
let gindirect_huge (node : 'a T.node) (ms : move_size) (rbase : int) (offset : int)
    (rtmp : int) (r : int) (is_store : bool) : pool Codegen.action =
  let access () =
    if is_store
    then olsr12u node (opstr12_sized ms) 0 rtmp r
    else olsr12u node (opldr12_sized ms) 0 rtmp r
  in
  let add_rtmp = oprrr_arith ADD lor (rbase lsl 16) lor (rtmp lsl 5) lor rtmp in
  match move_immediate_encoding_sized false offset with
  | Some movbase ->
      { size = 12; x = None; binary = (fun () ->
        [ w1 (movbase lor rtmp); w1 add_rtmp; w1 (access ()) ]
      )}
  | None ->
      { size = 12; x = Some (PoolOperand (Ast_asm.Int offset)); binary = (fun () ->
        [ gload_from_pool node rtmp; w1 add_rtmp; w1 (access ()) ]
      )}

(* claude: PCSZ (goken's own linkers/7l/l.h), the saved-link-register
 * slot size every real frame reserves -- same constant Rewrite7.ml's
 * own prologue/epilogue math uses (see its own `pcsz` binding). *)
let pcsz = 8

(* claude: real "(FP)"/"(SP)"-relative named local/param offset,
 * shared by every Entity(Local|Param) case below (address-of, and
 * the 6 register/immediate/float Move forms) -- e.g. real 7c -S
 * output for fmt/dofmt.c's own "fmt+8(FP),R11" (fmt is dofmt's 2nd
 * parameter).
 *
 * NOTE the naming: this port's own A.entity constructors are swapped
 * from their shared doc comment (Ast_asm.ml: "Param of ... (* FP *)
 * / Local of ... (* SP *)") -- ARM64's grammar (like ARM32's own
 * Parser_asm5.mly, identical mapping, already verified working)
 * binds TFP to `Local` and TSP to `Param`, so a real "(FP)"
 * reference (a genuine caller-frame PARAMETER in Plan9's own
 * convention) surfaces here as `A.Local`, and a real "(SP)"
 * reference (a genuine callee-frame LOCAL) surfaces as `A.Param` --
 * confirmed against the real -S output above ("fmt+8(FP)" parses to
 * `Entity (Local (Some "fmt", 8))`, and fmt genuinely is a
 * parameter, not a local variable).
 *
 * Formula needs the CALLEE's own TRUE frame size (the real amount SP
 * moved by at function entry), NOT env.autosize directly:
 * Rewrite7.ml's own TEXT mutation stores `autosize - pcsz` as the
 * TEXT node's declared size whenever a real frame exists (frame =
 * Some, see its "n.instr <- T.TEXT (..., autosize - pcsz)"), which
 * is what env.autosize reflects at codegen time (Layout7.ml's own
 * `autosize := size` from that same TEXT node); for a true leaf with
 * NO frame at all (only possible when the source's own declared size
 * is exactly 0 -- Rewrite7.ml's "leaf && autosize0 <= pcsz" case),
 * the TEXT node is left untouched, so env.autosize already IS the
 * (zero) true autosize, not "true - pcsz". env.autosize = 0
 * unambiguously identifies that one no-frame case: any function that
 * DOES get a real frame always has a true autosize that's a positive
 * multiple of 16 (STACKALIGN), so its own env.autosize = true - pcsz
 * is always >= 8, never 0.
 *
 * A true LOCAL (this port's `Param`) sits *within* this function's
 * own frame: true_autosize + off (off usually negative, e.g.
 * "rune-4(SP)", so that adding it back onto true_autosize lands
 * within this function's own frame). A true PARAMETER (this port's
 * `Local`) sits *above* this function's entire frame, in the
 * caller's own stack region: true_autosize + pcsz + off -- the extra
 * pcsz is the caller's OWN reserved link-register-save slot (every
 * real frame reserves one, including the caller's, since the callee
 * being called at all means the caller is never a leaf), same shape
 * as ARM32's real 5a (see Codegen5.ml's own "+4" comment on its
 * Local case) just with PCSZ=8 instead of 4.
 *
 * PREVIOUSLY WRONG in this port: the formula used to add env.autosize
 * directly (both cases), never reconstructing true_autosize at all --
 * silently correct only for the declared-size-0 leaf case (where
 * env.autosize already equals true_autosize), which is exactly why
 * it slipped past every regression fixture (none of them exercise a
 * real (FP)/(SP) reference at all -- see the new fp_offset.s/
 * sp_offset.s fixtures added alongside this fix) and even past the
 * full hello_libc closure *linking* successfully: the bug only
 * surfaces as a NULL-pointer segfault at actual native runtime
 * (dofmt reading a garbage "fmt" pointer 8 bytes short of where
 * vfprint actually wrote it). Tracked down by disassembling the real
 * failure (not re-derived from the ABI alone) and independently
 * confirmed against real goken with two hand-written probes: a leaf
 * callee's own "arg+0(FP)" resolves to the caller's SP-at-call + 8
 * (not +0), and a non-leaf caller's own named local "x-8(SP)"
 * resolves to that caller's post-prologue SP + true_autosize - 8
 * (not env.autosize - 8) -- see
 * docs/claude_notes/plan_hello_libc_linking.md. *)
let local_param_offset (env : Codegen.env) (ent : A.entity) : int =
  let true_autosize = if env.autosize = 0 then 0 else env.autosize + pcsz in
  match ent with
  | A.Local (_, off) -> true_autosize + pcsz + off
  | A.Param (_, off) -> true_autosize + off
  | A.Global _ -> raise (Impossible "Global handled via the SB-relative path above")

(*****************************************************************************)
(* The rules! *)
(*****************************************************************************)

let rules (env : Codegen.env) (init_data : T.addr option) (node : 'a T.node) =
  ignore init_data;
  match node.instr with
  (* Reusable *)
  | T.Virt _ | T.TEXT _ | T.WORD _ ->
      Codegen.default_rules env init_data node

  | T.I instr ->
    (match instr with

    (* --------------------------------------------------------------------- *)
    (* Arithmetic *)
    (* --------------------------------------------------------------------- *)

    (* case 1: op Rm,[Rn,]Rd *)
    | Arith (op, Reg (R rf), middle, (R rt)) ->
        let (R r) = middle ||| R rt in
        { size = 4; x = None; binary = (fun () ->
          [ w1 (oprrr_arith op lor (rf lsl 16) lor (r lsl 5) lor rt) ]
        )}
    (* case 53: AND/ORR/EOR $bimm,[Rn,]Rd -- bitmask immediate (e=64
     * cases only, see bitmask_immediate_encoding's own comment). *)
    | Arith ((AND_ | ORR | EOR as op), Imm i, middle, (R rt)) ->
        let (R r) = middle ||| R rt in
        (match bitmask_immediate_encoding op i r rt with
        | Some w -> { size = 4; x = None; binary = (fun () -> [ w1 w ]) }
        | None ->
            error node
              "TODO: AND/ORR/EOR immediate isn't a valid e=64 bitmask pattern (needs e<64 replication, deliberately not implemented -- see bitmask_immediate_encoding's comment -- or isn't representable as a bitmask immediate at all)")

    (* case 53, 32-bit-view: ANDW/ORRW/EORW $imm,[Rn,]Rd -- goken's own
     * real narrow-element-size (e<=32) bitmask-immediate encoder is
     * CONFIRMED BUGGY (see isbitcon/bitcon64_params's own comment
     * above, which documents the real "AND $0x0202020202020202,..."
     * miscompile found empirically against goken's real 7a/7l), so
     * rather than replicate goken's own broken mechanism this port
     * takes a genuinely different, xix-only path: materialize the
     * immediate into REGTMP via a 32-bit MOVZW/MOVNW (falling back to
     * the literal pool for anything that doesn't fit either lane),
     * then perform the operation via the already-working
     * register-register form (oprrr_arith, case 1) -- always
     * correct, unlike copying goken's own confirmed-wrong bytes for
     * this specific family. Found stress-testing real lib_core/libc
     * (fmt/fltfmt.c's real "ANDW $1024,R1" and siblings), see
     * docs/claude_notes/plan_hello_libc_linking.md. *)
    | Arith ((ANDW | ORRW | EORW as op), Imm i, middle, (R rt)) ->
        let (R r) = middle ||| R rt in
        let (R rtmp) = rTMP in
        (match move_immediate_encoding_sized true i with
        | Some base ->
            { size = 8; x = None; binary = (fun () ->
              [ w1 (base lor rtmp);
                w1 (oprrr_arith op lor (rtmp lsl 16) lor (r lsl 5) lor rt) ]
            )}
        | None ->
            { size = 8; x = Some (PoolOperand (Ast_asm.Int i)); binary = (fun () ->
              [ gload_from_pool node rtmp;
                w1 (oprrr_arith op lor (rtmp lsl 16) lor (r lsl 5) lor rt) ]
            )})

    (* case 2/4: ADD/SUB/ADDW/SUBW $imm,[Rn,]Rd -- addcon fast path
     * (see oaddi/isaddcon), falling back to the exact same REGTMP-
     * materialize-then-register-op substitute already used for
     * ANDW/ORRW/EORW just above whenever `i` is too big for the real
     * "addcon" shape (12 bits, optionally shifted left 12): goken's
     * own real fallback there is its own "$lcon" mechanism (a genuine
     * extended-register literal-pool address form this port doesn't
     * implement), so this is a deliberately different, xix-only
     * substitute rather than a claim of real 7l byte parity -- same
     * category as the store-immediate-to-memory expansion above.
     * Found stress-testing real lib_core/libc (port/frexp.c's real
     * "ADDW $268435456,R5", i.e. 1<<28). *)
    | Arith ((ADD | SUB | ADDW | SUBW as op), Imm i, middle, (R rt)) when not (isaddcon i) ->
        let (R r) = middle ||| R rt in
        let is_w = (match op with ADDW | SUBW -> true | _ -> false) in
        let (R rtmp) = rTMP in
        (match move_immediate_encoding_sized is_w i with
        | Some movbase ->
            { size = 8; x = None; binary = (fun () ->
              [ w1 (movbase lor rtmp);
                w1 (oprrr_arith op lor (rtmp lsl 16) lor (r lsl 5) lor rt) ]
            )}
        | None ->
            { size = 8; x = Some (PoolOperand (Ast_asm.Int i)); binary = (fun () ->
              [ gload_from_pool node rtmp;
                w1 (oprrr_arith op lor (rtmp lsl 16) lor (r lsl 5) lor rt) ]
            )})

    (* case 2/4: op $imm,[Rn,]Rd ("addcon" fast path only -- see oaddi) *)
    | Arith (op, Imm i, middle, (R rt)) ->
        let (R r) = middle ||| R rt in
        { size = 4; x = None; binary = (fun () ->
          [ w1 (oaddi node (opirr_addsub op) i r rt) ]
        )}

    (* case 8: shift by immediate (bitfield-move encoding) *)
    | Shift (op, Imm v, middle, (R rt)) ->
        let (R rf) = middle ||| R rt in
        { size = 4; x = None; binary = (fun () ->
          [ w1 (match op with
            | LSL -> opbfm opirr_ubfm ((64 - v) land 63) (63 - v) rf rt
            | LSR -> opbfm opirr_ubfm v 63 rf rt
            | ASR -> opbfm opirr_sbfm v 63 rf rt
            | ROR -> opextr opirr_extr v rf rf rt
            | LSLW -> opbfm opirr_ubfmw ((32 - v) land 31) (31 - v) rf rt
            | LSRW -> opbfm opirr_ubfmw v 31 rf rt
            | ASRW -> opbfm opirr_sbfmw v 31 rf rt
            | RORW -> opextr opirr_extrw v rf rf rt
          ) ]
        )}
    (* case 9: shift by register *)
    | Shift (op, Reg (R rf), middle, (R rt)) ->
        let (R r) = middle ||| R rt in
        { size = 4; x = None; binary = (fun () ->
          [ w1 (oprrr_shift op lor (rf lsl 16) lor (r lsl 5) lor rt) ]
        )}

    (* case 24/25: NEG/MVN Rn,Rd (plain-register form) -- Rn<<16, ZR
     * (31)<<5, Rd, same implicit-ZR-operand shape as CMP/CMN below. *)
    | Neg2 (op, (R rf), (R rt)) ->
        { size = 4; x = None; binary = (fun () ->
          [ w1 (oprrr_neg2 op lor (rf lsl 16) lor (31 lsl 5) lor rt) ]
        )}

    (* case 45: SXTW/UXTW Rn,Rd *)
    | Extend (op, (R rf), (R rt)) ->
        { size = 4; x = None; binary = (fun () ->
          [ w1 (oextend op rf rt) ]
        )}

    (* case 7: CMP/CMN *)
    | Cmp (op, Reg (R rf), (R rn)) ->
        { size = 4; x = None; binary = (fun () ->
          [ w1 (oprrr_cmp op lor (rf lsl 16) lor (rn lsl 5) lor 31) ]
        )}
    (* claude: same addcon-range fallback as ADD/SUB just above (goken's
     * real "$lcon" extended-register literal-pool form not
     * implemented; REGTMP-materialize + register-register CMP/CMN
     * substitute instead). Found stress-testing real lib_core/libc
     * (port/frexp.c's real "CMPW $65535,R5"). *)
    | Cmp (op, Imm i, (R rn)) when not (isaddcon i) ->
        let is_w = (match op with CMPW | CMNW -> true | CMP | CMN -> false) in
        let (R rtmp) = rTMP in
        (match move_immediate_encoding_sized is_w i with
        | Some movbase ->
            { size = 8; x = None; binary = (fun () ->
              [ w1 (movbase lor rtmp);
                w1 (oprrr_cmp op lor (rtmp lsl 16) lor (rn lsl 5) lor 31) ]
            )}
        | None ->
            { size = 8; x = Some (PoolOperand (Ast_asm.Int i)); binary = (fun () ->
              [ gload_from_pool node rtmp;
                w1 (oprrr_cmp op lor (rtmp lsl 16) lor (rn lsl 5) lor 31) ]
            )})
    | Cmp (op, Imm i, (R rn)) ->
        { size = 4; x = None; binary = (fun () ->
          [ w1 (oaddi node (opirr_cmp op) i rn 31) ]
        )}

    (* case 15: simple 3-operand MUL Rm,[Rn,]Rd (no accumulate) *)
    | ArithMul (op, (R rf), middle, (R rt)) ->
        let (R r) = middle ||| R rt in
        { size = 4; x = None; binary = (fun () ->
          [ w1 (oprrr_mul op lor (rf lsl 16) lor (31 lsl 10) lor (r lsl 5) lor rt) ]
        )}

    (* case 16: REM/REMW Rdivisor,[Rdividend,]Rdest -- 2 real
     * instructions (SDIV;MSUB), see orem's own comment. *)
    | Rem (op, (R rf), middle, (R rt)) ->
        let (R r) = middle ||| R rt in
        let sf, div_op = match op with
          | REM -> s64, 3 | REMW -> s32, 3
          | UREM -> s64, 2 | UREMW -> s32, 2
        in
        let (R rtmp) = rTMP in
        { size = 8; x = None; binary = (fun () ->
          let (o1, o2) = orem sf div_op rtmp rf r rt in
          [ w1 o1; w1 o2 ]
        )}

    (* --------------------------------------------------------------------- *)
    (* Floating point *)
    (* --------------------------------------------------------------------- *)

    (* case 54: FADDD Fm,[Fn,]Fd (dyadic float arith) *)
    | FArith (op, (FR rf), middle, (FR rt)) ->
        let (FR r) = middle ||| FR rt in
        { size = 4; x = None; binary = (fun () ->
          [ w1 (oprrr_farith op lor (rf lsl 16) lor (r lsl 5) lor rt) ]
        )}

    (* case 56: FCMPD Fm,Fn *)
    | FCmp (op, (FR rf), (FR rn)) ->
        { size = 4; x = None; binary = (fun () ->
          [ w1 (oprrr_fcmp op lor (rf lsl 16) lor (rn lsl 5)) ]
        )}

    (* --------------------------------------------------------------------- *)
    (* Memory / Move *)
    (* --------------------------------------------------------------------- *)

    (* case 24: MOV Rs,Rd (register-to-register) *)
    | Move (X_, Left (GReg (R rf)), GReg (R rt)) ->
        { size = 4; x = None; binary = (fun () ->
          [ w1 (gmov_reg_reg rf rt) ]
        )}

    (* claude: goken's real case 45 also covers "movT R,R -> sxtT R,R"
     * (the exact same table as SXTW/UXTW's own standalone mnemonics,
     * see Extend/oextend's own comment): MOVW (signed 32-bit
     * register-to-register move) is NOT a separate encoding at all,
     * it's a bare alias for SXTW's own opbfm-based one (goken's
     * asmout.c literally comments out its own would-be "case AMOVW:"
     * row in oprrr(), routing it through case 45 instead). MOVWU
     * (unsigned/zero-extending) is genuinely different: a real ORRW
     * Wd,WZR,Wf (32-bit OR against the zero register), which
     * naturally zero-extends into the destination's upper 32 bits on
     * real AArch64 -- goken's own oprrr(AMOVWU) is literally
     * oprrr(AORRW)'s exact same row. Found stress-testing real
     * lib_core/libc (fmt/dofmt.c's real "MOVW R1,R12"), see
     * docs/claude_notes/plan_hello_libc_linking.md. *)
    | Move (W_ A.S, Left (GReg (R rf)), GReg (R rt)) ->
        { size = 4; x = None; binary = (fun () -> [ w1 (oextend SXTW rf rt) ]) }
    | Move (W_ A.U, Left (GReg (R rf)), GReg (R rt)) ->
        { size = 4; x = None; binary = (fun () ->
          [ w1 (oprrr_arith ORRW lor (rf lsl 16) lor (31 lsl 5) lor rt) ]
        )}

    (* claude: MOVB/MOVBU/MOVH/MOVHU register-to-register -- same
     * case-45 "movT R,R -> sxtT/uxtT R,R" alias as MOVW above, but
     * UNLIKE MOVWU (which gets the ORRW shortcut, see just above)
     * MOVBU/MOVHU genuinely need the real bitfield-extract mechanism
     * (UBFM), not a plain register copy: an 8/16-bit zero-extend has
     * to clear bits above the extracted field, which a 32-bit-view
     * ORR can't do (it would leave bits [31:8]/[31:16] untouched,
     * only *adding* a zeroed top half at [63:32] for free) --
     * confirmed against asmout.c's own case 45 table, which groups
     * AMOVB/AMOVBU/AMOVH/AMOVHU with ASXTB/AUXTB/ASXTH/AUXTH under
     * the exact same opbfm(...) branches, unlike AMOVWU's own
     * separate oprrr()-based branch. Reuses opbfm/opirr_sbfm/
     * opirr_ubfm directly (Extend/oextend's own SXTW/UXTW machinery,
     * generalized to immediates 7/15 instead of 31) rather than
     * widening the parser-facing extend_opcode type for an encoding
     * these two mnemonics never surface under their own SXTB/UXTB/
     * SXTH/UXTH names in this port. Found stress-testing real
     * lib_core/libc (fmt/utf's real "MOVBU R11,R11"), see
     * docs/claude_notes/plan_hello_libc_linking.md. *)
    | Move (B_ A.S, Left (GReg (R rf)), GReg (R rt)) ->
        { size = 4; x = None; binary = (fun () -> [ w1 (opbfm opirr_sbfm 0 7 rf rt) ]) }
    | Move (B_ A.U, Left (GReg (R rf)), GReg (R rt)) ->
        { size = 4; x = None; binary = (fun () -> [ w1 (opbfm opirr_ubfm 0 7 rf rt) ]) }
    | Move (H_ A.S, Left (GReg (R rf)), GReg (R rt)) ->
        { size = 4; x = None; binary = (fun () -> [ w1 (opbfm opirr_sbfm 0 15 rf rt) ]) }
    | Move (H_ A.U, Left (GReg (R rf)), GReg (R rt)) ->
        { size = 4; x = None; binary = (fun () -> [ w1 (opbfm opirr_ubfm 0 15 rf rt) ]) }

    (* case 54 (monadic branch): FMOVS/FMOVD Fs,Fd (float register move) *)
    | Move ((FS_ | FD_ as ms), Left (GFReg (FR rf)), GFReg (FR rt)) ->
        { size = 4; x = None; binary = (fun () ->
          [ w1 (oprrr_fmovreg ms lor (rf lsl 5) lor rt) ]
        )}

    (* case 29: SCVTFS/SCVTFD/UCVTFWD Rs,Fd (int -> float) *)
    | Move ((SCVTF_S | SCVTF_D | UCVTF_WD | SCVTF_WD as ms), Left (GReg (R rf)), GFReg (FR rt)) ->
        { size = 4; x = None; binary = (fun () ->
          [ w1 (oprrr_fcvt ms lor (rf lsl 5) lor rt) ]
        )}
    (* case 29: FCVTZSS/FCVTZSD Fs,Rd (float -> int, truncating) *)
    | Move ((FCVTZS_S | FCVTZS_D | FCVTZU_WD | FCVTZS_WD as ms), Left (GFReg (FR rf)), GReg (R rt)) ->
        { size = 4; x = None; binary = (fun () ->
          [ w1 (oprrr_fcvt ms lor (rf lsl 5) lor rt) ]
        )}

    (* case 32: MOV $con,Rd -> movz/movn (see move_immediate_encoding's
     * own comment; no literal pool yet for anything that needs it) *)
    | Move (X_, Right (Int i), GReg (R rt)) ->
        (match move_immediate_encoding i with
        | Some base ->
            { size = 4; x = None; binary = (fun () -> [ w1 (base lor rt) ]) }
        | None ->
            (* case 12: movT $lcon,reg -- doesn't fit a direct
             * MOVZ/MOVN, needs the literal pool. *)
            { size = 4; x = Some (PoolOperand (Ast_asm.Int i)); binary = (fun () ->
              [ gload_from_pool node rt ]
            )})

    (* case 32, 32-bit-view: MOVW $con,Rd -> movzw/movnw -- see
     * move_immediate_encoding_sized's own comment. Found stress-
     * testing real lib_core/libc (fmt/utfrune.c's real "MOVW
     * $4,R5"), see docs/claude_notes/plan_hello_libc_linking.md. *)
    | Move ((W_ _), Right (Int i), GReg (R rt)) ->
        (match move_immediate_encoding_sized true i with
        | Some base ->
            { size = 4; x = None; binary = (fun () -> [ w1 (base lor rt) ]) }
        | None ->
            { size = 4; x = Some (PoolOperand (Ast_asm.Int i)); binary = (fun () ->
              [ gload_from_pool node rt ]
            )})

    (* claude: "FMOVD $con,Fd" -- always through the literal pool
     * (unlike the integer forms above, which try MOVZ/MOVN first):
     * goken's own real chipfloat-immediate mechanism is confirmed
     * dead code even in the reference implementation (see FArith's
     * own comment), so there's no "direct encoding" fast path to try
     * at all here, real or otherwise -- every float constant needs
     * the pool. See opldr_literal_fmov's own comment for why this
     * port implements it anyway (a genuinely different, working
     * mechanism from goken's own broken one). Only FD_ (double) --
     * FS_ (single) not yet needed by any real closure stress-tested
     * so far. *)
    | Move (FD_, Right (Float f), GFReg (FR rt)) ->
        { size = 4; x = Some (PoolOperand (Ast_asm.Float f)); binary = (fun () ->
          [ gload_from_pool_f node rt ]
        )}

    (* case 4 (C_AECON)/case 12 (C_LCON): MOV $sym(SB),Rd --
     * address-of-global. Caught the hard way (byte-diff against
     * goken): this does NOT always go through the literal pool --
     * span.c's own aclass() (D_STATIC/D_EXTERN's default case) takes
     * a direct "ADD $offset,RSB,Rt" fast path (same C_AECON/case-4
     * "addcon" shape as Arith's own $addcon immediate, reusing
     * oaddi/opirr_addsub below) whenever the resolved SB-relative
     * offset is BOTH nonzero AND fits ADD's addcon range (`isaddcon`)
     * -- confirmed directly: a global at data-offset 0 (nonzero check
     * fails) goes through the pool, but a global at a later,
     * small-enough offset (e.g. 8, following another global) uses the
     * direct ADD instead. The pool is only the fallback for offset=0
     * or an offset too large for addcon, not the universal case this
     * file previously (incorrectly) assumed. *)
    | Move (X_, Right (Address (Global (global, goffset))), GReg (R rt)) ->
        let final_offset =
          match Hashtbl.find_opt env.syms (T.symbol_of_global global) with
          | Some (T.SData2 (offset, _kind)) -> Some (offset + goffset)
          | Some (T.SText2 _) | None -> None
        in
        (match final_offset with
        | Some fo when fo <> 0 && isaddcon fo ->
            let (R rsb_i) = rSB in
            { size = 4; x = None; binary = (fun () ->
              [ w1 (oaddi node (opirr_addsub ADD) fo rsb_i rt) ]
            )}
        | _ ->
            { size = 4; x = Some (PoolOperand (Ast_asm.Address (Global (global, goffset))));
              binary = (fun () -> [ gload_from_pool node rt ]) }
        )

    (* claude: goken's real case 34 ("mov $lacon,R" -- address of a
     * named local/param, not its value) synthesizes a genuine 2-
     * instruction literal-pool sequence (omovlit + extended-register
     * ADD) for ANY offset, always -- unlike case 4's own addcon fast
     * path for SB-relative addresses just above, which only falls
     * back to the pool when the immediate doesn't fit. This port
     * takes the simpler, narrower route instead: the exact same
     * addcon-immediate ADD fast path as Address(Global) above,
     * reusing local_param_offset's own formula (see its own comment,
     * and the Entity(Local|Param) Move case below, for the swapped-
     * naming explanation) -- correct and sufficient for any
     * realistically-sized frame (isaddcon's own 12-bit-scaled-by-4096
     * range), with no literal-pool fallback for an offset too large
     * to fit (not yet needed by any real closure stress-tested so
     * far; would need genuinely new pool-operand plumbing to add,
     * unlike Address(Global)'s already-existing one). Found stress-
     * testing real lib_core/libc (fmt/rune.c's real "MOV
     * $rune-4(SP),R0"), see
     * docs/claude_notes/plan_hello_libc_linking.md. *)
    | Move (X_, Right (Address ((Local _ | Param _) as ent)), GReg (R rt)) ->
        let (R rsp_i) = rSP in
        let final_offset = local_param_offset env ent in
        if final_offset <> 0 && isaddcon final_offset
        then
          { size = 4; x = None; binary = (fun () ->
            [ w1 (oaddi node (opirr_addsub ADD) final_offset rsp_i rt) ]
          )}
        else
          error node (spf "TODO: address-of-local/param offset %d doesn't fit the addcon immediate range (needs literal pool, not yet implemented)" final_offset)

    (* claude: real "(FP)"/"(SP)"-relative named local/param access,
     * e.g. real 7c -S output for fmt/dofmt.c's own "fmt+8(FP),R11"
     * (fmt is dofmt's 2nd parameter). Resolves to the exact same
     * (rSP, real_offset) shape a plain Indirect(rbase,offset) memory
     * operand already is, then the exact same scaled-12-bit-or-
     * unscaled-9-bit machinery as case 20/21 just below (this port's
     * own new unscaled-9-bit fallback, not yet applied to the plain
     * Indirect cases below -- named locals/params are where an
     * unaligned offset actually turned up in practice). See
     * local_param_offset's own comment for the swapped-naming
     * explanation and the offset formula itself. *)
    | Move (ms, Left (GReg (R rf)), Entity ((A.Local _ | A.Param _) as ent)) ->
        let (R rsp_i) = rSP in
        let offset = local_param_offset env ent in
        let shift = scale_shift_of_size ms in
        { size = 4; x = None; binary = (fun () ->
          if offset land ((1 lsl shift) - 1) <> 0
          then [ w1 (olsr9s node (opstr9_sized ms) offset rsp_i rf) ]
          else [ w1 (olsr12u node (opstr12_sized ms) (offset asr shift) rsp_i rf) ]
        )}
    | Move (ms, Left (Entity ((A.Local _ | A.Param _) as ent)), GReg (R rt)) ->
        let (R rsp_i) = rSP in
        let offset = local_param_offset env ent in
        let shift = scale_shift_of_size ms in
        { size = 4; x = None; binary = (fun () ->
          if offset land ((1 lsl shift) - 1) <> 0
          then [ w1 (olsr9s node (opldr9_sized ms) offset rsp_i rt) ]
          else [ w1 (olsr12u node (opldr12_sized ms) (offset asr shift) rsp_i rt) ]
        )}

    (* claude: the float-register siblings of the two Entity(Local|
     * Param) cases just above -- same offset formula, same scaled/
     * unscaled fallback, just GFReg instead of GReg. Found stress-
     * testing real lib_core/libc (fmt/dofmt.c's real "FMOVD
     * f+0(FP),F7"). *)
    | Move ((FS_ | FD_ as ms), Left (GFReg (FR rf)), Entity ((A.Local _ | A.Param _) as ent)) ->
        let (R rsp_i) = rSP in
        let offset = local_param_offset env ent in
        let shift = scale_shift_of_size ms in
        { size = 4; x = None; binary = (fun () ->
          if offset land ((1 lsl shift) - 1) <> 0
          then [ w1 (olsr9s node (opstr9_sized ms) offset rsp_i rf) ]
          else [ w1 (olsr12u node (opstr12_sized ms) (offset asr shift) rsp_i rf) ]
        )}
    | Move ((FS_ | FD_ as ms), Left (Entity ((A.Local _ | A.Param _) as ent)), GFReg (FR rt)) ->
        let (R rsp_i) = rSP in
        let offset = local_param_offset env ent in
        let shift = scale_shift_of_size ms in
        { size = 4; x = None; binary = (fun () ->
          if offset land ((1 lsl shift) - 1) <> 0
          then [ w1 (olsr9s node (opldr9_sized ms) offset rsp_i rt) ]
          else [ w1 (olsr12u node (opldr12_sized ms) (offset asr shift) rsp_i rt) ]
        )}

    (* claude: "MOVT $con,O(Rbase)" -- store an immediate directly to
     * memory. NOT real 7a syntax at all: confirmed absent from
     * goken's own optab.c (no AMOVW/AMOV row pairs a C_MOVCON/C_LCON
     * "from" with any O(R)-shaped "to" -- every real row needing a
     * memory destination takes C_REG as its "from", never a
     * constant), unlike CASE/BCASE elsewhere in this port, which
     * *does* have real grammar. Real hardware has no store-immediate
     * instruction at all, so this is a genuine 2-instruction xix-only
     * expansion (materialize into REGTMP via the exact same
     * move_immediate_encoding(_sized) machinery as a register
     * destination, then store REGTMP with the exact same scaled-or-
     * unscaled machinery as case 20 just below), not a claim of real
     * 7a byte parity -- same category as ARM32's own IndirectShift.
     * Found stress-testing real lib_core/libc (fmt/dofmt.c's real
     * "MOVW $0,24(R31)"), see
     * docs/claude_notes/plan_hello_libc_linking.md. *)
    | Move (ms, Right (Int i), Indirect ((R rbase), offset)) ->
        let is_w = (match ms with X_ -> false | _ -> true) in
        (match move_immediate_encoding_sized is_w i with
        | None -> error node "TODO: store-immediate value doesn't fit a direct MOVZ/MOVN (needs literal pool, not yet implemented)"
        | Some movbase ->
            let (R rtmp) = rTMP in
            let shift = scale_shift_of_size ms in
            { size = 8; x = None; binary = (fun () ->
              let store =
                if offset land ((1 lsl shift) - 1) <> 0
                then olsr9s node (opstr9_sized ms) offset rbase rtmp
                else olsr12u node (opstr12_sized ms) (offset asr shift) rbase rtmp
              in
              [ w1 (movbase lor rtmp); w1 store ]
            )}
        )

    (* claude: same "MOVT $con,O(Rbase)" xix-only expansion just above,
     * for a *named* local/param destination instead of a raw
     * Indirect -- see that case's own comment, and the Entity
     * (Local|Param) Move case further above for the offset formula/
     * swapped-naming explanation. Found stress-testing real
     * lib_core/libc (fmt/dofmt.c's real "MOVW $0,w-68(SP)"). *)
    | Move (ms, Right (Int i), Entity ((A.Local _ | A.Param _) as ent)) ->
        let (R rsp_i) = rSP in
        let offset = local_param_offset env ent in
        let is_w = (match ms with X_ -> false | _ -> true) in
        (match move_immediate_encoding_sized is_w i with
        | None -> error node "TODO: store-immediate value doesn't fit a direct MOVZ/MOVN (needs literal pool, not yet implemented)"
        | Some movbase ->
            let (R rtmp) = rTMP in
            let shift = scale_shift_of_size ms in
            { size = 8; x = None; binary = (fun () ->
              let store =
                if offset land ((1 lsl shift) - 1) <> 0
                then olsr9s node (opstr9_sized ms) offset rsp_i rtmp
                else olsr12u node (opstr12_sized ms) (offset asr shift) rsp_i rtmp
              in
              [ w1 (movbase lor rtmp); w1 store ]
            )}
        )

    (* case 20: MOV(B[U]|H[U]|W[U])? Rs,O(Rbase) -> STR, any size --
     * 3-tier (scaled-12-bit / unscaled-9-bit / gindirect_huge), same
     * shape as the SB-relative fast path further below. *)
    | Move (ms, Left (GReg (R rf)), Indirect ((R rbase), offset)) ->
        let shift = scale_shift_of_size ms in
        let (R rtmp) = rTMP in
        if offset land ((1 lsl shift) - 1) = 0 && offset >= 0 && (offset asr shift) < (1 lsl 12)
        then { size = 4; x = None; binary = (fun () ->
                 [ w1 (olsr12u node (opstr12_sized ms) (offset asr shift) rbase rf) ]
               )}
        else if offset >= -256 && offset <= 255
        then { size = 4; x = None; binary = (fun () ->
                 [ w1 (olsr9s node (opstr9_sized ms) offset rbase rf) ]
               )}
        else gindirect_huge node ms rbase offset rtmp rf true
    (* case 21: MOV(B[U]|H[U]|W[U])? O(Rbase),Rd -> LDR, any size --
     * same 3-tier shape. *)
    | Move (ms, Left (Indirect ((R rbase), offset)), GReg (R rt)) ->
        let shift = scale_shift_of_size ms in
        let (R rtmp) = rTMP in
        if offset land ((1 lsl shift) - 1) = 0 && offset >= 0 && (offset asr shift) < (1 lsl 12)
        then { size = 4; x = None; binary = (fun () ->
                 [ w1 (olsr12u node (opldr12_sized ms) (offset asr shift) rbase rt) ]
               )}
        else if offset >= -256 && offset <= 255
        then { size = 4; x = None; binary = (fun () ->
                 [ w1 (olsr9s node (opldr9_sized ms) offset rbase rt) ]
               )}
        else gindirect_huge node ms rbase offset rtmp rt false

    (* case 20/21, float memory form: FMOVS/FMOVD Fs,O(Rbase) /
     * FMOVS/FMOVD O(Rbase),Fd -- same 3-tier machinery as the integer
     * sizes above, just the "V" bit set (see ldstr12u_size_v_opc). *)
    | Move ((FS_ | FD_ as ms), Left (GFReg (FR rf)), Indirect ((R rbase), offset)) ->
        let shift = scale_shift_of_size ms in
        let (R rtmp) = rTMP in
        if offset land ((1 lsl shift) - 1) = 0 && offset >= 0 && (offset asr shift) < (1 lsl 12)
        then { size = 4; x = None; binary = (fun () ->
                 [ w1 (olsr12u node (opstr12_sized ms) (offset asr shift) rbase rf) ]
               )}
        else if offset >= -256 && offset <= 255
        then { size = 4; x = None; binary = (fun () ->
                 [ w1 (olsr9s node (opstr9_sized ms) offset rbase rf) ]
               )}
        else gindirect_huge node ms rbase offset rtmp rf true
    | Move ((FS_ | FD_ as ms), Left (Indirect ((R rbase), offset)), GFReg (FR rt)) ->
        let shift = scale_shift_of_size ms in
        let (R rtmp) = rTMP in
        if offset land ((1 lsl shift) - 1) = 0 && offset >= 0 && (offset asr shift) < (1 lsl 12)
        then { size = 4; x = None; binary = (fun () ->
                 [ w1 (olsr12u node (opldr12_sized ms) (offset asr shift) rbase rt) ]
               )}
        else if offset >= -256 && offset <= 255
        then { size = 4; x = None; binary = (fun () ->
                 [ w1 (olsr9s node (opldr9_sized ms) offset rbase rt) ]
               )}
        else gindirect_huge node ms rbase offset rtmp rt false

    (* case 20/21 (SB-relative fast path): "MOV Rf,sym(SB)" / "MOV
     * sym(SB),Rd" -- store/load the *value* at a global (as opposed
     * to "MOV $sym(SB),Rd", address-of-global, which always goes
     * through the pool -- see the Right(Address ...) arms above and
     * Ast_asm7.ml's prelude comment). REGSB (x28) is set up to point
     * at data-offset 0 with no bias at all (confirmed empirically,
     * unlike ARM32/RISC-V's BIG-biased SB register), so the fast path
     * is just a plain scaled-12-bit-unsigned STR/LDR off x28 -- long
     * offsets that don't fit aren't implemented yet (would need a
     * pool-loaded-offset + register-offset STR/LDR, same shape as
     * ARM32's case 30/31). *)
    | Move (ms, Left (GReg (R rf)), Entity (A.Global (global, goffset))) ->
        let v = Hashtbl.find env.syms (T.symbol_of_global global) in
        (match v with
        | T.SText2 _ -> error node "TODO: storing to a TEXT symbol"
        | T.SData2 (offset, _kind) ->
            let final_offset = offset + goffset in
            let shift = scale_shift_of_size ms in
            let (R rsb_i) = rSB in
            let (R rtmp) = rTMP in
            if final_offset land ((1 lsl shift) - 1) = 0
            then { size = 4; x = None; binary = (fun () ->
                     [ w1 (olsr12u node (opstr12_sized ms) (final_offset asr shift) rsb_i rf) ]
                   )}
            else if final_offset >= -256 && final_offset <= 255
            then { size = 4; x = None; binary = (fun () ->
                     [ w1 (olsr9s node (opstr9_sized ms) final_offset rsb_i rf) ]
                   )}
            else gsbrel_huge node ms global goffset rtmp rf true
        )
    | Move (ms, Left (Entity (A.Global (global, goffset))), GReg (R rt)) ->
        let v = Hashtbl.find env.syms (T.symbol_of_global global) in
        (match v with
        | T.SText2 _ -> error node "TODO: loading the value at a TEXT symbol"
        | T.SData2 (offset, _kind) ->
            let final_offset = offset + goffset in
            let shift = scale_shift_of_size ms in
            let (R rsb_i) = rSB in
            let (R rtmp) = rTMP in
            if final_offset land ((1 lsl shift) - 1) = 0
            then { size = 4; x = None; binary = (fun () ->
                     [ w1 (olsr12u node (opldr12_sized ms) (final_offset asr shift) rsb_i rt) ]
                   )}
            else if final_offset >= -256 && final_offset <= 255
            then { size = 4; x = None; binary = (fun () ->
                     [ w1 (olsr9s node (opldr9_sized ms) final_offset rsb_i rt) ]
                   )}
            else gsbrel_huge node ms global goffset rtmp rt false
        )

    (* case 20/21, float SB-relative fast path *)
    | Move ((FS_ | FD_ as ms), Left (GFReg (FR rf)), Entity (A.Global (global, goffset))) ->
        let v = Hashtbl.find env.syms (T.symbol_of_global global) in
        (match v with
        | T.SText2 _ -> error node "TODO: storing to a TEXT symbol"
        | T.SData2 (offset, _kind) ->
            let final_offset = offset + goffset in
            let shift = scale_shift_of_size ms in
            let (R rsb_i) = rSB in
            let (R rtmp) = rTMP in
            if final_offset land ((1 lsl shift) - 1) = 0
            then { size = 4; x = None; binary = (fun () ->
                     [ w1 (olsr12u node (opstr12_sized ms) (final_offset asr shift) rsb_i rf) ]
                   )}
            else if final_offset >= -256 && final_offset <= 255
            then { size = 4; x = None; binary = (fun () ->
                     [ w1 (olsr9s node (opstr9_sized ms) final_offset rsb_i rf) ]
                   )}
            else gsbrel_huge node ms global goffset rtmp rf true
        )
    | Move ((FS_ | FD_ as ms), Left (Entity (A.Global (global, goffset))), GFReg (FR rt)) ->
        let v = Hashtbl.find env.syms (T.symbol_of_global global) in
        (match v with
        | T.SText2 _ -> error node "TODO: loading the value at a TEXT symbol"
        | T.SData2 (offset, _kind) ->
            let final_offset = offset + goffset in
            let shift = scale_shift_of_size ms in
            let (R rsb_i) = rSB in
            let (R rtmp) = rTMP in
            if final_offset land ((1 lsl shift) - 1) = 0
            then { size = 4; x = None; binary = (fun () ->
                     [ w1 (olsr12u node (opldr12_sized ms) (final_offset asr shift) rsb_i rt) ]
                   )}
            else if final_offset >= -256 && final_offset <= 255
            then { size = 4; x = None; binary = (fun () ->
                     [ w1 (olsr9s node (opldr9_sized ms) final_offset rsb_i rt) ]
                   )}
            else gsbrel_huge node ms global goffset rtmp rt false
        )

    (* case 23: MOV Rf,-16(Rbase)! / MOV Rf,(Rbase)16! -- pre/post-index
     * writeback store *)
    | Move (X_, Left (GReg (R rf)), PreIndex ((R rbase), offset)) ->
        { size = 4; x = None; binary = (fun () ->
          [ w1 (ldrstr_pp node opstrpp_mov false offset rbase rf) ]
        )}
    | Move (X_, Left (GReg (R rf)), PostIndex ((R rbase), offset)) ->
        { size = 4; x = None; binary = (fun () ->
          [ w1 (ldrstr_pp node opstrpp_mov true offset rbase rf) ]
        )}
    (* case 22: MOV -16(Rbase)!,Rt / MOV (Rbase)16!,Rt -- pre/post-index
     * writeback load *)
    | Move (X_, Left (PreIndex ((R rbase), offset)), GReg (R rt)) ->
        { size = 4; x = None; binary = (fun () ->
          [ w1 (ldrstr_pp node opldrpp_mov false offset rbase rt) ]
        )}
    | Move (X_, Left (PostIndex ((R rbase), offset)), GReg (R rt)) ->
        { size = 4; x = None; binary = (fun () ->
          [ w1 (ldrstr_pp node opldrpp_mov true offset rbase rt) ]
        )}

    (* --------------------------------------------------------------------- *)
    (* Control flow *)
    (* --------------------------------------------------------------------- *)

    (* case 5: B label / BL label *)
    | B { contents = (Absolute _) } ->
        { size = 4; x = None; binary = (fun () ->
          [ w1 (opbra_b lor ((branch_delta node) land 0x3FFFFFF)) ]
        )}
    | BL { contents = (Absolute _) } ->
        { size = 4; x = None; binary = (fun () ->
          [ w1 (opbra_bl lor ((branch_delta node) land 0x3FFFFFF)) ]
        )}
    (* case 6: B (Rn) / BL (Rn) -- indirect through register *)
    | B { contents = (IndirectJump (R rt)) } ->
        { size = 4; x = None; binary = (fun () -> [ w1 (opbrr_b lor (rt lsl 5)) ]) }
    | BL { contents = (IndirectJump (R rt)) } ->
        { size = 4; x = None; binary = (fun () -> [ w1 (opbrr_bl lor (rt lsl 5)) ]) }

    (* case 7 (branch variant): BEQ label etc *)
    | Bxx (cond, { contents = (Absolute _) }) ->
        { size = 4; x = None; binary = (fun () ->
          [ w1 (opbcc (int_of_condition cond)
                lor (((branch_delta node) land 0x7FFFF) lsl 5)) ]
        )}

    (* case 8's own LTYPE: CBZ/CBNZ Rt,label *)
    | CBxx (nonzero, (R rt), { contents = (Absolute _) }) ->
        { size = 4; x = None; binary = (fun () ->
          [ w1 (opcbz nonzero
                lor (((branch_delta node) land 0x7FFFF) lsl 5)
                lor rt) ]
        )}

    (* case 40: TBZ/TBNZ $bit,Rt,label *)
    | TBxx (nonzero, bit, (R rt), { contents = (Absolute _) }) ->
        { size = 4; x = None; binary = (fun () ->
          [ w1 (opirr_tbz nonzero lor (tbz_bitfield bit)
                lor (((branch_delta node) land 0x3FFF) lsl 5)
                lor rt) ]
        )}

    (* case 62: CASE Rv,Rt -> adr tab,Rt; movw Rt[Rv<<2],REGTMP;
     * add Rt,REGTMP; br (REGTMP). `tab` is always 16 bytes (4
     * instructions) past this CASE itself -- see Ast_asm7.ml's
     * CaseJump comment. The `last_case_pc` update is deferred into
     * this thunk (not done eagerly above) so it only ever fires when
     * this node's real bytes are actually being emitted (`gen`'s own
     * pass), never during a size-only query elsewhere -- see
     * last_case_pc's own comment for why that matters. *)
    | CaseJump ((R rv), (R rt)) ->
        { size = 16; x = None; binary = (fun () ->
          last_case_pc := node.real_pc;
          let (R rtmp) = rTMP in
          [ w1 (oadr 16 rt);
            w1 (ocase_load rv rt rtmp);
            w1 (oprrr_arith ADD lor (rt lsl 16) lor (rtmp lsl 5) lor rtmp);
            w1 (opbrr_b lor (rtmp lsl 5)) ]
        )}

    (* case 63: BCASE label -- not a real instruction, one raw 32-bit
     * table entry: target.real_pc - (last CASE's real_pc + 16). See
     * last_case_pc's own comment; "missing CASE" mirrors goken's own
     * diag() for a BCASE with no preceding CASE. *)
    | BCase { contents = (Absolute _) } ->
        { size = 4; x = None; binary = (fun () ->
          if !last_case_pc < 0
          then error node "BCASE with no preceding CASE (missing CASE)";
          match node.branch with
          | None -> raise (Impossible "resolving should have set the branch field")
          | Some ndst -> [ w1 (ndst.real_pc - (!last_case_pc + 16)) ]
        )}

    (* RET / RET Rn -- defaults to RLINK (X30), goken's own default for
     * a bare "RET". *)
    | RET None ->
        { size = 4; x = None; binary = (fun () ->
          let (R rt) = rLINK in [ w1 (opbrr_ret lor (rt lsl 5)) ]
        )}
    | RET (Some (R rt)) ->
        { size = 4; x = None; binary = (fun () -> [ w1 (opbrr_ret lor (rt lsl 5)) ]) }

    (* --------------------------------------------------------------------- *)
    (* Conditional select *)
    (* --------------------------------------------------------------------- *)

    (* case 18: CSEL/CSINC/CSINV/CSNEG (and CINC/CINV/CNEG's alias
     * form) cond,Rn,[Rm,]Rd -- Rm absent means the 2-register alias
     * shape (goken inverts cond and reuses Rn as Rm, see this file's
     * oprrr_condsel comment). *)
    | CondSel (op, cond, (R rn), Some (R rm), (R rt)) ->
        { size = 4; x = None; binary = (fun () ->
          [ w1 (oprrr_condsel op lor (rm lsl 16)
                lor ((int_of_condition cond) lsl 12) lor (rn lsl 5) lor rt) ]
        )}
    | CondSel (op, cond, (R rn), None, (R rt)) ->
        { size = 4; x = None; binary = (fun () ->
          [ w1 (oprrr_condsel op lor (rn lsl 16)
                lor (((int_of_condition cond) lxor 1) lsl 12) lor (rn lsl 5) lor rt) ]
        )}

    (* case 18: CSET/CSETM cond,Rd -- both source-register positions
     * default to ZR, cond inverted (same shape as CondSel's 2-register
     * alias form above, just with no source register at all). *)
    | CondSet (op, cond, (R rt)) ->
        { size = 4; x = None; binary = (fun () ->
          [ w1 (oprrr_condset op lor (31 lsl 16)
                lor (((int_of_condition cond) lxor 1) lsl 12) lor (31 lsl 5) lor rt) ]
        )}

    (* --------------------------------------------------------------------- *)
    (* Atomics *)
    (* --------------------------------------------------------------------- *)

    (* case 58: LDXR/LDAXR (Rn),Rt *)
    | LoadExcl (acquire, (R rn), (R rt)) ->
        { size = 4; x = None; binary = (fun () ->
          [ w1 (opload_excl acquire lor (0x1F lsl 16) lor (rn lsl 5)
                lor (0x1F lsl 10) lor rt) ]
        )}
    (* case 59: STXR/STLXR Rt,(Rn),Rs *)
    | StoreExcl (release, (R rt), (R rn), (R rs)) ->
        { size = 4; x = None; binary = (fun () ->
          [ w1 (opstore_excl release lor (rs lsl 16) lor (rn lsl 5)
                lor (0x1F lsl 10) lor rt) ]
        )}

    (* --------------------------------------------------------------------- *)
    (* System *)
    (* --------------------------------------------------------------------- *)

    (* case 10: SVC [$con] -- goken's own case ignores the immediate's
     * actual value for the Linux syscall ABI purpose (syscall number is
     * in R8, not encoded in the instruction) but still packs it into the
     * instruction if given, same as ARM32/MIPS's SWI/SYSCALL. *)
    | SVC i ->
        { size = 4; x = None; binary = (fun () ->
          [ w1 (opimm_svc lor ((i land 0xffff) lsl 5)) ]
        )}

    (* case 51: DMB/DSB/ISB $imm *)
    | Barrier (op, i) ->
        { size = 4; x = None; binary = (fun () ->
          [ w1 (opirr_barrier op lor ((i land 0xF) lsl 8)) ]
        )}

    (* --------------------------------------------------------------------- *)
    (* Not yet implemented -- see this file's prelude comment *)
    (* --------------------------------------------------------------------- *)
    | Move _ ->
        error node "Codegen7: TODO: move operand combination not handled yet"
    | B { contents = (Relative _ | LabelUse _ | SymbolJump _) }
    | BL { contents = (Relative _ | LabelUse _ | SymbolJump _) }
    | Bxx (_, { contents = (Relative _ | LabelUse _ | SymbolJump _ | IndirectJump _) })
    | CBxx (_, _, { contents = (Relative _ | LabelUse _ | SymbolJump _ | IndirectJump _) })
    | TBxx (_, _, _, { contents = (Relative _ | LabelUse _ | SymbolJump _ | IndirectJump _) })
    | BCase { contents = (Relative _ | LabelUse _ | SymbolJump _ | IndirectJump _) } ->
        raise (Impossible
          "branch operand should have been resolved to Absolute by now")
    )

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

let size_of_instruction (env : Codegen.env) (node : 'a T.node) : int (* a multiple of 4 *) * pool option =
  let action = rules env None node in
  action.size, action.x

let gen (symbols2 : T.symbol_table2) (config : Exec_file.linker_config)
   (cg : 'a T.code_graph) : T.word list =

  let res = ref [] in
  let autosize = ref 0 in
  let pc = ref config.init_text in
  last_case_pc := -1 (* fresh per program, see last_case_pc's own comment *);

  cg |> T.iter (fun n ->
    let {size; binary; x = _} =
        rules Codegen.{ syms = symbols2; autosize = !autosize }
        config.init_data n
    in
    let instrs = binary () in

    if n.real_pc <> !pc
    then raise (Impossible "Phase error, layout inconsistent with codegen");
    if List.length instrs * 4 <> size
    then raise (Impossible (spf "size of rule does not match #instrs at %s"
                              (T.s_of_loc n.n_loc)));

    let xs : Bits.int32 list = instrs |> List.map Assoc.sort_by_val_highfirst in

    (* claude: `land 0xffffffff` recovers the unsigned int expected
     * downstream (Types.word/`res`) -- Int32.to_int alone sign-extends
     * a word with its top bit set (routine for machine instructions)
     * into a negative int; ocaml-light's Printf also has no "%lx" to
     * format an Int32.t directly for the debug dump below. *)
    let int_of_bits32 (x : Bits.int32) : int =
      Int32.to_int (Bits.int_of_bits32 x) land 0xffffffff in

    if !Flags.debug_gen
    then begin
      Logs.app (fun m -> m " %.8x: %s"
                 !pc
                  (xs |> List.map (fun x -> spf "%.8x" (int_of_bits32 x))
                      |> String.concat " "));
    end;

    let xs = xs |> List.map int_of_bits32 in
    res |> Stack_.push xs;

    pc := !pc + size;
    (match n.instr with
    | T.TEXT (_, _, size) -> autosize := size;
    | _ -> ()
    );
  );

  !res |> List.rev |> List.flatten
