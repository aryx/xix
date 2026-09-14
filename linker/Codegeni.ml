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

module Str = Re_str
open Ast_asm
open Ast_asmi

module T = Types
open Codegen

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* RISC-V (RV32) code generation.
 *
 * The 'case <n>: ...' comments below refer to code in il/asm.c so one
 * can easily check the corresponding C code that was used as model
 * for the OCaml code (mirrors the convention in Codegen5.ml/
 * Codegenv.ml).
 *
 * Unlike ARM (needs a literal pool) or MIPS (needs "lu+or"), a large
 * RISC-V constant is always materialized inline at its use site via
 * LUI (+ADDI for the low 12 bits) -- see cases 8/9/20 below -- so
 * there is no pool/splicing mechanism here at all.
 *
 * goken's il also applies instruction *compression* (RVC, 16-bit
 * encodings for eligible instructions, see il/compress.c) unless
 * given `-c`; this port never emits compressed instructions, so the
 * differential harness always passes `-c` to goken's il to compare
 * apples to apples -- see scripts/diff-riscv.sh and
 * docs/claude_notes/riscv_port.md.
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
let error (node : 'a T.node) (s : string) =
  failwith
    (spf "%s at %s on %s" s (T.s_of_loc node.n_loc)
        (Typesi.show_instr node.instr))
let int_of_bits (n : 'a T.node) (x : Bits.int32) : int =
  try Bits.int_of_bits32 x with
  | Failure s -> error n s

(*****************************************************************************)
(* Constants and helpers *)
(*****************************************************************************)

(* claude: BIG, ported from goken's linkers/il/l.h. RSB (aka SB, aka
 * gp/x3 in the real ISA) is set up at program start to point BIG
 * bytes into the data segment (see "setSB" below), so a *later*
 * MOVW $sym(SB) can reach it with one `ADDI rd, RSB, offset-BIG`
 * instead of loading the full 32-bit absolute address via LUI+ADDI --
 * see cases 11 (fast path) and 20 (slow/absolute path) below.
 *
 * Unlike MIPS (where the analogous BIG is 0, permanently disabling
 * this fast path in goken itself -- see Codegenv.ml) RISC-V's BIG is
 * a real, live value: 2048, which is exactly the magnitude of a
 * 12-bit signed immediate's range. That's not a coincidence -- it's
 * what makes the fast-path encodability check below a plain range
 * check instead of ARM's bit-rotation search (immrot): any
 * offset-BIG that still fits a signed 12-bit immediate is reachable.
 *)
let big = 2048

let offset_to_SB x = x - big

(* claude: the exact condition il/span.c's aclass() uses (the D_ADDR/
 * SDATA case) is `instoffset >= -BIG && instoffset < BIG`, i.e. the
 * signed 12-bit immediate range -- equivalently, "fits in ADDI's
 * imm field". The separate `!= 0` exclusion (checked at the call
 * site below, not here) is what keeps `MOVW $setSB(SB), RSB` itself
 * from trying to use the fast path, which would be circular: RSB
 * isn't set up yet at that point.
 *)
let fits_addi_imm x = x >= -big && x < big

(*****************************************************************************)
(* Instruction encoding helpers *)
(*****************************************************************************)
(* Standard RV32I instruction formats (I/S/B/U/J-type); mirrors
 * goken's OP_* macros in il/asm.c, just spelled out as Bits.t
 * (value, bit-offset) lists like Codegen5.ml/Codegenv.ml do, instead
 * of C bit-shift expressions.
 *)

let op_opimm = 0x13 (* ADDI/SLTI/etc *)
let op_op = 0x33 (* ADD/SUB/AND/OR/etc (register-register) -- goken's OOP *)
let op_lui = 0x37
(* claude: AUIPC (PC-relative "add upper immediate"); riscv64/ojl uses
 * this instead of LUI for absolute-address computations -- see
 * gen_pcrelative and case 20 below. *)
let op_auipc = 0x17
let op_system = 0x73 (* ECALL/EBREAK *)

(* I-type: imm[31:20] rs1[19:15] funct3[14:12] rd[11:7] opcode[6:0] *)
let op_itype opcode funct3 (R rs1) (R rd) (imm : int) : Bits.t =
  [(opcode, 0); (rd, 7); (funct3, 12); (rs1, 15); (imm land 0xfff, 20)]

(* U-type: imm[31:12] rd[11:7] opcode[6:0]. `imm20` is the already
 * upper-20-bit-shifted value (as goken's `v&0xFFFFF000` is).
 *)
let op_utype opcode (R rd) (imm20 : int) : Bits.t =
  [(opcode, 0); (rd, 7); ((imm20 lsr 12) land 0xfffff, 12)]

(* R-type: funct7[31:25] rs2[24:20] rs1[19:15] funct3[14:12] rd[11:7]
 * opcode[6:0] -- goken's OP_R(rs1,rs2,rd) (case 0). *)
let op_rtype opcode funct3 funct7 (R rs1) (R rs2) (R rd) : Bits.t =
  [(opcode, 0); (rd, 7); (funct3, 12); (rs1, 15); (rs2, 20); (funct7, 25)]

let op_fp = 0x53 (* OP-FP major opcode -- goken's OOP_FP *)
(* claude: case 17's own shape (goken's `OP_RF(rs1,rs2,rd,rm)` ==
 * `OPX | rm<<12 | rd<<7 | R(rs1)<<15 | R(rs2)<<20 | o->param<<25` --
 * same R-type field layout as op_rtype, just with the funct3 slot
 * repurposed as `rm` (rounding mode) and the "rs2" register-number
 * slot repurposed as a format selector, not a real second operand
 * register. The `o->param<<25` funct7 term is easy to miss reading
 * asm.c's case 17 body alone -- it's buried in the OP_RF macro
 * definition itself, a screen away -- so every (funct7, rs2_sel, rm)
 * triple below was verified empirically against real goken (`ia`/
 * `il`), not just derived from the C source: MOVFD/MOVDF -> FCVT.D.S
 * (funct7=0x21,rs2=0)/FCVT.S.D (0x20,1); MOVFW/MOVDW -> FCVT.W.S
 * (0x60,0)/FCVT.W.D (0x61,0), rm=1 (round-to-zero, goken's own
 * `o->a3==C_REG` check -- always true here since the destination is
 * always a plain register for these two); MOVWF/MOVWD -> FCVT.S.W
 * (0x68,0)/FCVT.D.W (0x69,0), rm=7 (dynamic rounding, destination is
 * always a float register for these two). *)
(* claude: takes raw ints (not `(R _)`/`(FR _)`) since rs1/rd can be
 * either register file here, depending on conversion direction --
 * the caller destructures whichever constructor applies. *)
let op_rftype (funct7 : int) (rs2_sel : int) (rm : int) (rs1 : int) (rd : int) : Bits.t =
  [(op_fp, 0); (rd, 7); (rm, 12); (rs1, 15); (rs2_sel, 20); (funct7, 25)]

(* claude: (funct3, funct7) for case 0 (register-register) and case 1
 * (shift-immediate, which reuses funct3/the sign of funct7 as its
 * "which shift" selector -- see op_itype_shift below) -- goken's
 * optab.c func3/param columns. Only the RV32-native (`w option =
 * None`) forms; the explicit-32-bit-on-RV64 `*W` variants (ADDW,
 * SLLW, etc -- a genuinely different opcode family, OOP_32 not OOP)
 * are left as a follow-up, same scoping as everywhere else this
 * session: fail loudly rather than silently emit the wrong opcode. *)
let oprrr_arith_opcode (op : arith_opcode) : int * int =
  match op with
  | ADD None -> 0, 0
  | SUB None -> 0, 0x20
  | SLL None -> 1, 0
  | SLT S -> 2, 0
  | SLT U -> 3, 0
  | XOR -> 4, 0
  | SRL None -> 5, 0
  | SRA None -> 5, 0x20
  | OR -> 6, 0
  | AND -> 7, 0
  | ADD (Some _) | SUB (Some _) | SLL (Some _) | SRL (Some _) | SRA (Some _) ->
      failwith "TODO:oprrr_arith_opcode RV64 *W ops"

(* claude: goken's case 9/20 pattern for materializing an absolute
 * 32-bit value v into rd: LUI the upper 20 bits, rounding up (adding
 * 0x1000) if bit 11 of v is set, since ADDI's 12-bit immediate is
 * *signed* -- ADDI rd,rd,(v&0xFFF as a signed 12-bit value) would
 * otherwise subtract instead of add when that low part's bit 11 is
 * set. Then ADDI the (now-consistent) low 12 bits into rd itself.
 *)
let gen_absolute_via (opcode : int) (rd : reg) (v : int) : Bits.t list =
  let v = if v land 0x800 <> 0 then v + 0x1000 else v in
  [ op_utype opcode rd v;
    op_itype op_opimm 0 rd rd (v land 0xfff);
  ]
let gen_absolute (rd : reg) (v : int) : Bits.t list = gen_absolute_via op_lui rd v

(* claude: same instruction pair as gen_absolute, but AUIPC instead of
 * LUI -- goken's riscv64 (thechar='j') uses this for case 12/13/18/20
 * (see il/asm.c's `thechar == 'j' ? OP_UP(...) : OP_U(...)`) so that
 * the materialized address stays correct as a *delta from this
 * instruction's own pc* rather than a 32-bit-truncated absolute
 * value. Caller is responsible for passing that delta as `v`. *)
let gen_pcrelative (rd : reg) (v : int) : Bits.t list = gen_absolute_via op_auipc rd v

(* claude: case 12/13's own LUI(-only)/AUIPC helper -- same rounding
 * as gen_absolute_via above (bit 11 forces a round-up so the
 * following signed 12-bit field adds rather than subtracts), but the
 * low 12 bits are handed back to the caller instead of being folded
 * in via a second ADDI: case 12/13 fold them directly into the
 * store/load instruction's own 12-bit immediate field instead,
 * saving the separate ADDI case 9/20's "materialize an address into
 * a register" shape needs (goken's own `o1=OP_U(REGTMP,v); v&=0xFFF;
 * o2=OP_S(REGTMP,...,v)`, no separate ADDI at all). *)
let gen_upper_and_low_via (opcode : int) (rd : reg) (v : int) : Bits.t * int =
  let v = if v land 0x800 <> 0 then v + 0x1000 else v in
  (op_utype opcode rd v, v land 0xfff)

(* S-type: imm[31:25] rs2[24:20] rs1[19:15] funct3[14:12] imm[11:7]
 * opcode[6:0] -- goken's OP_S(rs1,rs2,imm) (case 6, STORE opcode
 * 0x23), shared by SB/SH/SW/SD (see op_itype's LOAD counterpart) and,
 * via an explicit `opcode` param, FSW/FSD too (STORE-FP opcode 0x27
 * -- same S-type layout, `rs2` is just a float register there). *)
let op_stype opcode funct3 (R rs1) (R rs2) (imm : int) : Bits.t =
  [(opcode, 0); (imm land 0x1f, 7); (funct3, 12);
   (rs1, 15); (rs2, 20); ((imm lsr 5) land 0x7f, 25)]

(* claude: case 6/7 (small offset, goken's `if(v < -BIG || v >= BIG)
 * diag(...)` range check) / case 15/16 (large offset, "mov r,L(s)"/
 * "mov L(s),r" -- an arbitrary base register, unlike case 12/13's
 * SB-specific slow path: no INITDAT/BIG bias, just the raw offset).
 * Shared by both the store and load sides, and each of their W__/V__
 * and B_/H_ variants -- goken's own case 15/16 is fully generic over
 * SB/SH/SW/SD/LB/LH/LW/LD/LBU/LHU too (confirmed via optab.c: the
 * same C_LOREG class every one of case 6/7's own C_SOREG mnemonics
 * also has a row for), so this generalizes the exact same way case
 * 6/7 already did. Large-offset case materializes the offset's upper
 * bits into REGTMP via LUI (same rounding as case 9/20/12/13's own
 * `gen_upper_and_low_via`), adds the base register into REGTMP (goken's
 * plain `OP_ADD`, no funct7/funct3 needed beyond the implicit ADD
 * opcode), then stores/loads through REGTMP with the low 12 bits
 * folded into the instruction's own immediate field. *)
let gen_store ?(opcode = 0x23) (_node : 'a T.node) (funct3 : int) (rbase : reg) (rf : reg) (offset : int) =
  if fits_addi_imm offset
  then { size = 4; x = None; binary = (fun () -> [ op_stype opcode funct3 rbase rf offset ]) }
  else
    { size = 12; x = None; binary = (fun () ->
      let (lui_bits, low12) = gen_upper_and_low_via op_lui rTMP offset in
      [ lui_bits; op_rtype op_op 0 0 rbase rTMP rTMP; op_stype opcode funct3 rTMP rf low12 ]
    )}
let gen_load ?(opcode = 0x03) (_node : 'a T.node) (funct3 : int) (rbase : reg) (rt : reg) (offset : int) =
  if fits_addi_imm offset
  then { size = 4; x = None; binary = (fun () -> [ op_itype opcode funct3 rbase rt offset ]) }
  else
    { size = 12; x = None; binary = (fun () ->
      let (lui_bits, low12) = gen_upper_and_low_via op_lui rTMP offset in
      [ lui_bits; op_rtype op_op 0 0 rbase rTMP rTMP; op_itype opcode funct3 rTMP rt low12 ]
    )}

let op_branch = 0x63 (* BEQ/BNE/BLT/BGE/BLTU/BGEU -- goken's OBRANCH *)
let op_jal = 0x6f (* goken's OJAL *)

(* B-type: imm[12|10:5] rs2[24:20] rs1[19:15] funct3[14:12]
 * imm[4:1|11] opcode[6:0] -- goken's OP_B(rs1,rs2,imm) macro, ported
 * bit-scatter-for-bit-scatter rather than re-derived from the ISA
 * manual's field layout, to stay byte-for-byte faithful. *)
let op_btype funct3 (R rs1) (R rs2) (imm : int) : Bits.t =
  [ (op_branch, 0);
    ((imm asr 11) land 0x1, 7);
    ((imm asr 1) land 0xf, 8);
    (funct3, 12);
    (rs1, 15);
    (rs2, 20);
    ((imm asr 5) land 0x3f, 25);
    ((imm asr 12) land 0x1, 31);
  ]

(* J-type: imm[20|10:1|11|19:12] rd[11:7] opcode[6:0] -- goken's
 * OP_J(rd,imm) macro, same bit-for-bit porting approach as OP_B. *)
let op_jtype (R rd) (imm : int) : Bits.t =
  [ (op_jal, 0);
    (rd, 7);
    ((imm asr 12) land 0xff, 12);
    ((imm asr 11) land 0x1, 20);
    ((imm asr 1) land 0x3ff, 21);
    ((imm asr 20) land 0x1, 31);
  ]

(* claude: (funct3) for case 3's branch conditions -- goken's
 * optab.c func3 column. GT/LE (b_condition's own AST constructors)
 * have no direct hardware encoding -- real RISC-V has no such
 * instructions -- but goken's own assembler DOES accept the
 * "BLE"/"BGT"/"BLEU"/"BGTU" mnemonics (confirmed: `assemblers/ia/
 * lex.c` maps all four to real tokens/opcodes); its own *linker*
 * rewrites them into the reversed-relation LT/GE form at encode time
 * (`linkers/il/obj.c`, case ABGT/ABGTU/ABLE/ABLEU). This port's own
 * `Bxx` codegen case (below) already remaps GT->LT/LE->GE before
 * ever calling this function, so `Bxx`'s own `cond` is always one of
 * the six cases handled here -- the GT/LE catch-all stays only as a
 * defensive "should be unreachable" guard. *)
let opirr_bxx_funct3 (c : b_condition) : int =
  match c with
  | EQ -> 0
  | NE -> 1
  | LT A.S -> 4
  | GE A.S -> 5
  | LT A.U -> 6
  | GE A.U -> 7
  | GT _ | LE _ ->
      raise (Impossible "Bxx's own codegen case remaps GT/LE to LT/GE \
                          before this is ever called")

(* claude: RISC-V has no branch-delay slot (unlike MIPS -- see
 * riscv_port.md), so a branch/jump's immediate is just a
 * plain PC-relative delta, no -4/-8 bias to account for. `node.
 * real_pc` and the branch target's `real_pc` are both absolute (not
 * goken's text-relative raw `pc`), but since this is a *difference*
 * the constant INITTEXT offset cancels out either way -- same
 * reasoning already used for case 20's riscv64/AUIPC delta. *)
let branch_delta (node : 'a T.node) : int =
  match node.branch with
  | None -> raise (Impossible "resolving should have set the branch field")
  | Some ndst -> ndst.real_pc - node.real_pc

(* claude: case 4's own J-type immediate is 21 bits wide but always
 * even (imm[20|10:1|11|19:12], LSB implicitly 0) -- a signed
 * -2^20..2^20-2 byte range. goken's own assembler picks case 18 (a
 * far-branch fallback: materialize the absolute target via LUI/AUIPC
 * into REGTMP, then JALR through it, same 2-instruction shape as
 * case 9/20's own big-constant/address story) whenever a JAL/JMP/
 * JALR-to-label target falls outside this range -- NOT implemented
 * here (a genuine assembler-level "does this reach" decision, not
 * just an isolated encoder function, closer in scope to ARM32/MIPS's
 * own multi-pass branch-range stories than a single case). Given
 * this port's own "loud error over silently-wrong bytes" policy,
 * `fits_jal_range` guards case 4's 3 call sites so an out-of-range
 * target errors instead of silently truncating -- see
 * riscv_port.md for the deferred case 18 writeup. *)
let fits_jal_range (delta : int) : bool =
  delta >= - (1 lsl 20) && delta < (1 lsl 20) && delta land 1 = 0

(*****************************************************************************)
(* The rules! *)
(*****************************************************************************)
(* conventions:
 * - rf = register from (p->from.reg in il)
 * - rt = register to (p->to.reg in il)
 *)

(* claude: goken's own linkers/il/span.c formula for D_AUTO/D_PARAM
 * (the "x-8(SP)"/"y+4(FP)" pseudo-addressing Rewritei.ml's own
 * TEXT-rewrite comment flagged as "Future Local/Param-relative
 * addressing (case 12-16, not yet implemented) must add ptrsize
 * itself at the point of computing an offset, same as goken's own
 * span.c `instoffset = autosize + a->offset + ptrsize`" -- this is
 * that "future" implementation. Two subtleties: (1) this port's own
 * `Param`/`Local` AST names are swapped relative to goken's own
 * D_PARAM/D_AUTO naming -- `Param` comes from the "SP" token (=
 * goken's D_AUTO, no +ptrsize) and `Local` comes from the "FP" token
 * (= goken's D_PARAM, +ptrsize) -- confirmed against Codegenv.ml's
 * own MIPS precedent (`Param -> autosize+off`, `Local ->
 * autosize+4+off`, "4" being MIPS's own ptrsize). (2) env.autosize
 * (as stored by Rewritei.ml's own TEXT mutation, `n.instr <-
 * T.TEXT(..., padded_size)`, and read back via Layouti.ml) is
 * `padded_size`, NOT goken's own "autosize" (the real SP-adjust
 * amount, `padded_size + ptrsize`) -- reconstructed here the same
 * way Codegen7.ml's own ARM64 "true_autosize" is, except when
 * genuinely 0 (case 1, no frame at all). NOT reconstructed correctly
 * for the one narrow edge case of a non-leaf function with a
 * *declared* $0 frame accessing its own arguments via (FP) --
 * Rewritei.ml's own `forced_size_when_declared_zero` bumps the real
 * SP-adjust to 2*ptrsize for exactly that case, indistinguishable
 * here from a genuine no-frame leaf (both show env.autosize=0); no
 * real closure stress-tested so far hits it. *)
let ptrsize_i (is_64 : bool) = if is_64 then 8 else 4
let true_autosize (is_64 : bool) (env : Codegen.env) =
  if env.autosize = 0 then 0 else env.autosize + ptrsize_i is_64

let resolve_entity (is_64 : bool) (env : Codegen.env) (e : A.entity) : gen =
  match e with
  | A.Param (_, off) -> Indirect (rSP, true_autosize is_64 env + off)
  | A.Local (_, off) ->
      Indirect (rSP, true_autosize is_64 env + off + ptrsize_i is_64)
  | A.Global _ -> Entity e

let resolve_gen (is_64 : bool) (env : Codegen.env) (g : gen) : gen =
  match g with
  | Entity ((A.Param _ | A.Local _) as e) -> resolve_entity is_64 env e
  | GReg _ | Indirect _ | Entity (A.Global _) -> g

let resolve_vgen (is_64 : bool) (env : Codegen.env) (v : vgen) : vgen =
  match v with
  | Gen g -> Gen (resolve_gen is_64 env g)
  | GFReg _ -> v

(* claude: applied once, uniformly, to every `gen`/`vgen` operand
 * before the big match below dispatches on instruction shape -- so
 * every existing (and future) `Indirect`/`Entity(Global)` match arm
 * automatically also handles the "off(SP)"/"off(FP)" pseudo-frame
 * spelling, without needing a parallel Param/Local-specific arm per
 * instruction shape. `Bxx`'s own `gen` is included for uniformity
 * even though no real closure stress-tested so far uses anything but
 * a plain register there. *)
let resolve_entities (is_64 : bool) (env : Codegen.env) (i : instr) : instr =
  let g = resolve_gen is_64 env and v = resolve_vgen is_64 env in
  match i with
  | Move1 (sz, Either.Left g1, g2) -> Move1 (sz, Either.Left (g g1), g g2)
  | Move1 (sz, Either.Right x, g2) -> Move1 (sz, Either.Right x, g g2)
  | Move2 (sz, Either.Left v1, v2) -> Move2 (sz, Either.Left (v v1), v v2)
  | Move2 (sz, Either.Right x, v2) -> Move2 (sz, Either.Right x, v v2)
  | Bxx (cond, g1, rf, b) -> Bxx (cond, g g1, rf, b)
  (* claude: "SUB $imm,[R,]D" -- real RISC-V has no immediate-subtract
   * opcode; goken's own linker (linkers/il/obj.c, case ASUB/ASUBW)
   * rewrites a constant-operand SUB into ADD with the immediate
   * negated (`p->from.offset = -p->from.offset; p->as = AADD`) at an
   * early, generic rewrite stage, before any of the SIZE/encoding-
   * specific case dispatch -- so real "SUB $imm,Rd" is never actually
   * seen by asm.c's own case-9-style ADDI/LUI+ADDI logic at all, it's
   * silently ADD-with-negated-immediate throughout. Mirrored here at
   * the same kind of early, generic point, letting the existing `ADD
   * None, Imm _` arm below handle both. Found stress-testing real
   * lib_core/libc (rt0.s's own real "SUB $12,SP" stack-alignment
   * prologue). *)
  | Arith (SUB None, Imm i, middle, rt) -> Arith (ADD None, Imm (-i), middle, rt)
  | Arith _ | ArithMul _ | ArithF _ | FCVTFF _ | FCVTFI _ | FCVTIF _
  | CmpF _ | LUI _ | FENCE_I | JMP _ | JAL _ | JALR _ | JALRI _
  | ECALL | BREAK | SYS | CSR _ -> i

(* claude: is_64 is only ever read inside a `binary` thunk (never
 * during the sizing pass -- see size_of_instruction below), mirroring
 * how init_data is threaded; it's riscv64/ojl's equivalent of goken's
 * global `thechar == 'j'` check. NOTE: `resolve_entities` above is a
 * partial exception to that invariant -- its Local/Param offset
 * computation genuinely depends on is_64 (ptrsize 4 vs 8), which can
 * in turn flip `fits_addi_imm`'s own size branch. size_of_instruction
 * always fakes is_64=false (see its own comment below), so this is
 * only actually consistent between the sizing and binary-generation
 * passes on riscv32 (is_64 always false there in practice); a real
 * riscv64 Local/Param offset that straddles the BIG=2048 boundary
 * only because of the +4-vs-+8 ptrsize difference could in principle
 * size-mismatch between the two passes -- not yet hit by any real
 * closure (riscv64's own hello_libc effort hasn't started). *)
let rec rules (is_64 : bool)
    (env : Codegen.env) (init_data : T.addr option) (node : 'a T.node) =
  match node.instr with
  (* Reusable *)
  | T.Virt _ | T.TEXT _ | T.WORD _ ->
      Codegen.default_rules env init_data node

  | T.I instr0 ->
    let instr = resolve_entities is_64 env instr0 in
    (match instr with

    (* --------------------------------------------------------------------- *)
    (* Arithmetic *)
    (* --------------------------------------------------------------------- *)

    (* case 0:		/* add S,[R,]D */ *)
    (* claude: register-register arith. goken's default-middle-reg
     * rule (asm.c's `if(r==NREG) ... default: r=p->to.reg`) matches
     * the existing `middle ||| rt` convention already used by case 2
     * below. OP_R(rs1,rs2,rd) = OP_R(r, p->from.reg, p->to.reg) --
     * note rs1 is the *middle* operand and rs2 the *from* one, not
     * the other way around (confirmed via `il -a`: "ADD R1,R2,R3"
     * encodes rs1=R2(middle), rs2=R1(from), rd=R3). *)
    | Arith (((ADD None | SUB None | SLL None | SRL None | SRA None
              | SLT _ | XOR | OR | AND) as op), Reg rf, middle, rt) ->
        let r = middle ||| rt in
        let (funct3, funct7) = oprrr_arith_opcode op in
        { size = 4; x = None; binary = (fun () ->
          [ op_rtype op_op funct3 funct7 r rf rt ]
        )}

    (* claude: real RISC-V M-extension (MUL/DIV/DIVU/REM/REMU) --
     * standard funct7=0x01 (the fixed "M-extension" marker, same
     * major OP opcode 0x33 as plain ADD/SUB) with funct3 selecting
     * the operation (0=MUL,4=DIV,5=DIVU,6=REM,7=REMU -- the standard
     * RISC-V ISA encoding, not goken-specific). Same `middle`/`from`/
     * `to` -> rs1/rs2/rd mapping as the base `Arith` reg-reg case
     * just above (confirmed by hand-decoding real goken bytes for
     * "DIV R5,R6,R7": rs1=R6(middle), rs2=R5(from), rd=R7(to) --
     * genuinely worth checking separately from plain ADD/SUB since
     * DIV/REM aren't commutative, so a left-right swap here would
     * silently compute the wrong VALUE, not just wrong bytes for an
     * equivalent value). The 2-register in-place form ("DIVU Rs,Rd"
     * => Rd=Rd/Rs) defaults `middle` to the destination, mirroring
     * plain `Arith`'s own identical default. MUL_ (opcode MUL, no
     * sign) not wired -- no real closure needs it, only the *_ /REM_
     * signed/unsigned forms do. Found stress-testing real
     * lib_core/libc (port/vlrt.c's own 64-bit-division helpers). *)
    | ArithMul (op, rf, middle, rt) ->
        let r = middle ||| rt in
        let funct3 = (match op with
          | MUL -> 0
          | DIV (None, A.S) -> 4 | DIV (None, A.U) -> 5
          | REM (None, A.S) -> 6 | REM (None, A.U) -> 7
          | DIV (Some _, _) | REM (Some _, _) ->
              failwith "Codegeni: ArithMul: *W (RV64 32-bit-view) \
                        variants not wired, no real closure needs them yet"
        ) in
        { size = 4; x = None; binary = (fun () ->
          [ op_rtype op_op funct3 0x01 r rf rt ]
        )}

    (* case 1:		/* slli $I,[R,]D */ *)
    (* claude: shift-by-immediate. Same rd/rs1 shape as case 2's
     * ADDI, but the immediate field packs the shift amount (bits
     * [4:0], goken masks 0x3F for RV64's wider shamt but only
     * [4:0] matters on RV32) together with a fixed selector bit
     * (bit 10, i.e. `param<<5` in goken, which is 0 for SLLI/SRLI
     * or 0x20<<5 for SRAI) that lands in the immediate's own
     * bits [11:5] -- the real ISA's funct7 field for shift-immediate
     * specifically. *)
    | Arith (((SLL None | SRL None | SRA None) as op), Imm i, middle, rt) ->
        let r = middle ||| rt in
        let (funct3, funct7) = oprrr_arith_opcode op in
        { size = 4; x = None; binary = (fun () ->
          [ op_itype op_opimm funct3 r rt ((i land 0x3f) lor (funct7 lsl 5)) ]
        )}

    (* case 2:		/* addi $I,[R,]D */ *)
    | Arith (ADD None, Imm i, middle, rt) ->
        let r = middle ||| rt in
        if fits_addi_imm i
        then
          { size = 4; x = None; binary = (fun () ->
            [ op_itype op_opimm 0 r rt i ]
          )}
        else
          (* case 14: lui L1,T; addi $L0,T,T; add T,r,d -- see the
           * generalized AND/OR/XOR arm below for the full comment. *)
          { size = 12; x = None; binary = (fun () ->
            let (lui_bits, low12) = gen_upper_and_low_via op_lui rTMP i in
            [ lui_bits; op_itype op_opimm 0 rTMP rTMP low12;
              op_rtype op_op 0 0 r rTMP rt ]
          )}

    (* case 2 (generalized): andi/ori/xori/slti/sltiu $I,[R,]D --
     * goken's optab reuses the *same* funct3 for these immediate
     * forms as their register-register counterparts (oprrr_arith_
     * opcode above), the real ISA's OP-IMM family; only SUB has no
     * immediate counterpart (ADDI with a negated immediate covers
     * that), and SLL/SRL/SRA's immediate forms are case 1 above
     * (a different bit layout: shift amount, not a plain 12-bit
     * signed immediate). *)
    | Arith (((AND | OR | XOR | SLT _) as op), Imm i, middle, rt) ->
        let r = middle ||| rt in
        let (funct3, _) = oprrr_arith_opcode op in
        if fits_addi_imm i
        then
          { size = 4; x = None; binary = (fun () ->
            [ op_itype op_opimm funct3 r rt i ]
          )}
        else
          (match op with
          | AND | OR | XOR ->
              (* case 14: lui L1,T; addi $L0,T,T; op T,r,d -- goken's
               * optab.c only has a C_LCON row for ADD/AND/OR/XOR
               * (confirmed: SLT/SLTU/SUB/SLL/SRL/SRA have none), each
               * reached the same way as case 9/20's own big-constant
               * materialization (LUI+ADDI into REGTMP), followed by
               * the real op register-register using REGTMP as the
               * "from" operand -- `OP_RO(r,REGTMP,rt)`'s own encoding
               * has no funct7 term at all (always 0), which is
               * exactly why only these 4 (whose register-register
               * funct7 is already 0) are reachable here; SUB/SRA's
               * own funct7=0x20 genuinely can't be expressed this
               * way, matching goken's own reference. *)
              { size = 12; x = None; binary = (fun () ->
                let (lui_bits, low12) = gen_upper_and_low_via op_lui rTMP i in
                [ lui_bits; op_itype op_opimm 0 rTMP rTMP low12;
                  op_rtype op_op funct3 0 r rTMP rt ]
              )}
          | SLT _ ->
              error node "TODO: SLT/SLTU immediate out of 12-bit range (no C_LCON row for this op in goken's own optab)"
          | ADD _ | SUB _ | SLL _ | SRL _ | SRA _ ->
              raise (Impossible "unreachable: outer pattern already restricts op to AND/OR/XOR/SLT"))

    (* case 8:		/* lui	I,D */ *)
    (* claude: standalone LUI -- goken's asm.c case 8 takes the raw
     * immediate as-is (`v = p->from.offset; o1 = OP_U(p->to.reg, v)`),
     * no rounding, unlike case 9's MOVW-immediate expansion (which
     * rounds to compensate for the ADDI that follows it -- there is
     * no following ADDI here, so no rounding is needed or wanted). *)
    | LUI (i, rt) ->
        { size = 4; x = None; binary = (fun () ->
          [ op_utype op_lui rt i ]
        )}

    (* --------------------------------------------------------------------- *)
    (* Floating point conversion *)
    (* --------------------------------------------------------------------- *)

    (* case 17: fcvt S,D -- see op_rftype's own comment for the
     * (funct7, rs2_sel, rm) triple per direction. *)
    | FCVTFF (MOVFD, (FR fs), (FR fd)) ->
        { size = 4; x = None; binary = (fun () -> [ op_rftype 0x21 0 7 fs fd ]) }
    | FCVTFF (MOVDF, (FR fs), (FR fd)) ->
        { size = 4; x = None; binary = (fun () -> [ op_rftype 0x20 1 7 fs fd ]) }
    | FCVTFI (MOVFW, (FR fs), (R rt)) ->
        { size = 4; x = None; binary = (fun () -> [ op_rftype 0x60 0 1 fs rt ]) }
    | FCVTFI (MOVDW, (FR fs), (R rt)) ->
        { size = 4; x = None; binary = (fun () -> [ op_rftype 0x61 0 1 fs rt ]) }
    | FCVTIF (MOVWF, (R rs), (FR fd)) ->
        { size = 4; x = None; binary = (fun () -> [ op_rftype 0x68 0 7 rs fd ]) }
    | FCVTIF (MOVWD, (R rs), (FR fd)) ->
        { size = 4; x = None; binary = (fun () -> [ op_rftype 0x69 0 7 rs fd ]) }
    (* claude: MOVUF/MOVUD -- unsigned siblings of MOVWF/MOVWD, same
     * funct7 (0x68/0x69), rs2_sel=1 instead of 0 (see optab.c's own
     * adjacent "int->float"/"uint->float" rows). *)
    | FCVTIF (MOVUF, (R rs), (FR fd)) ->
        { size = 4; x = None; binary = (fun () -> [ op_rftype 0x68 1 7 rs fd ]) }
    | FCVTIF (MOVUD, (R rs), (FR fd)) ->
        { size = 4; x = None; binary = (fun () -> [ op_rftype 0x69 1 7 rs fd ]) }

    (* claude: "MOVF Fs,Fd"/"MOVD Fs,Fd" -- a plain register move, real
     * RISC-V's own "FSGNJ.S/D Fd,Fs,Fs" self-sign-inject idiom (copy
     * Fs's own sign bit into itself, a no-op arithmetically, so the
     * whole value round-trips unchanged) -- confirmed against real
     * `linkers/il/optab.c`'s own "AMOVF/AMOVD, C_FREG,C_FREG ->
     * OOP_FP" row (funct7=0x10 float / 0x11 double, funct3=0 selects
     * FSGNJ specifically, as opposed to FSGNJN/FSGNJX for NEG/ABS).
     * Reuses `op_rftype` -- its own `rs2_sel` parameter is genuinely
     * just the rs2 register-number *field*, not always a fixed FCVT
     * selector constant the way case 17's own six rows above use it;
     * here it's the *same* source register as rs1, matching the real
     * self-sign-inject encoding. Reached via the generic Move2
     * (F__/D__) path (see the GFReg comment in Ast_asmi.ml), not a
     * dedicated constructor -- goken's own grammar doesn't give this
     * shape a separate mnemonic from the memory-access forms either. *)
    | Move2 (F__, Left (GFReg (FR fs)), GFReg (FR fd)) ->
        { size = 4; x = None; binary = (fun () -> [ op_rftype 0x10 fs 0 fs fd ]) }
    | Move2 (D__, Left (GFReg (FR fs)), GFReg (FR fd)) ->
        { size = 4; x = None; binary = (fun () -> [ op_rftype 0x11 fs 0 fs fd ]) }

    (* claude: "MOVD $const,Fd" -- a bare double-precision float
     * literal. Real goken's own linker (linkers/il/obj.c, case AMOVD)
     * rewrites this into a synthesized global data symbol (an ADATA
     * blob holding the raw IEEE754 bits, deduped by hex value) plus
     * an ordinary SB-relative load -- not replicated here (would need
     * this port's own linker to synthesize NEW global symbols mid-
     * codegen, a much bigger undertaking). Instead: RV32's D
     * extension keeps F registers 64 bits wide but has no FMV.D.X (no
     * way to move a 64-bit value directly from a GP register pair
     * into an F register -- that instruction only exists on RV64,
     * with its 64-bit GP registers) -- genuinely requires going
     * through memory on RV32. Materializes each 32-bit half into
     * RTMP (same LUI+ADDI-or-plain-ADDI shape as case 9's own integer
     * immediate) and stores it below the current SP (temporarily
     * adjusting SP by -8/+8 around the sequence), then FLDs from
     * there -- a real, standard technique (not goken's own, which
     * this port's own linker doesn't replicate), but genuinely
     * correct on real hardware, and this port's own binaries are
     * simple flat ELF executables with no signal handlers that could
     * ever observe the transiently-adjusted SP. NOT byte-identical
     * to real goken (which uses a completely different, global-
     * symbol-based mechanism) -- there is no goken reference to
     * match against here anyway for the specific files that need
     * this (build-c-program.py's own `patch_nonchipfloat_constants`
     * replaces genuinely dead-code float literals with 0.5 so they
     * assemble/link at all, never executed for real). *)
    | Move2 (D__, Right (Float f), GFReg (FR fd)) ->
        let bits = Int64.bits_of_float f in
        let lo = Int64.to_int (Int64.logand bits 0xFFFFFFFFL) in
        let hi = Int64.to_int (Int64.shift_right_logical bits 32) in
        let materialize (rd : reg) (v : int) : int * (unit -> Bits.t list) =
          if fits_addi_imm v
          then 4, (fun () -> [ op_itype op_opimm 0 rZERO rd v ])
          else 8, (fun () -> gen_absolute rd v)
        in
        let (lo_size, lo_bin) = materialize rTMP lo in
        let (hi_size, hi_bin) = materialize rTMP hi in
        { size = 4 + lo_size + 4 + hi_size + 4 + 4 + 4; x = None; binary = (fun () ->
          [ op_itype op_opimm 0 rSP rSP (-8) ] @ lo_bin () @
          [ op_stype 0x23 2 rSP rTMP 0 ] @ hi_bin () @
          [ op_stype 0x23 2 rSP rTMP 4;
            op_itype 0x07 3 rSP (R fd) 0;
            op_itype op_opimm 0 rSP rSP 8 ]
        )}

    (* claude: real float/double arithmetic (FADD/FSUB/FMUL.S/D) --
     * confirmed against real `linkers/il/optab.c`'s own "AADDD/ASUBD/
     * AMULD, C_FREG,C_FREG -> OOP_FP" rows: funct7 selects both the
     * operation and precision (ADD.S=0x00/ADD.D=0x01, SUB.S=0x04/
     * SUB.D=0x05, MUL.S=0x08/MUL.D=0x09 -- low bit is the precision,
     * matching real RISC-V's own F-extension encoding), funct3/rm is
     * always 7 (dynamic rounding). Only the 3-explicit-register form
     * ("ADDD Fa,Fb,Fc") is wired -- the 2-register in-place form
     * ("ADDD Fa,Fb" => Fb=Fb+Fa) isn't, no real closure needs it yet.
     * `from`/`middle`/`to` map to rs2/rs1/rd -- NOT the "obvious"
     * left-to-right rs1/rs2/rd reading. Confirmed by hand-decoding
     * real goken's own encoded bytes for "SUBD F0,F28,F0": the real
     * instruction word has rs1=F28(middle), rs2=F0(from), rd=F0(to)
     * -- an earlier attempt assumed left-to-right (rs1=from,
     * rs2=middle) and produced a byte-identical *size* but wrong
     * *content*, caught by diffing raw instruction words against
     * real ia/il output, not by trusting the derivation. Same
     * asymmetric convention as `Bxx`'s own `(middle,rf)` mapping. *)
    (* claude: 2-register in-place form ("ADDD Fa,Fb" => Fb=Fb+Fa) --
     * real goken's own grammar (assemblers/ia/a.y's `LFLT3 drreg ','
     * freg ',' drreg` production) has NO 2-operand alternative for
     * this instruction family at all (confirmed: real `ia` rejects
     * "ADDD F2,F0" outright as a syntax error) -- yet real -S output
     * genuinely contains this exact 2-operand spelling (found in
     * port/vlrt.c.s and port/frexp.c.s, both already in the "goken
     * can't reassemble its own -S output" set for unrelated reasons).
     * A 6th instance of this whole effort's running "-S print
     * artifact" bug family: the compiler's real internal `Prog` most
     * likely has an explicit middle register equal to the
     * destination (matching the SAME "middle defaults to `to`"
     * convention already established and byte-verified for the
     * *integer* `Arith` case just above -- goken's own asm.c: `if(r==
     * NREG) ... default: r=p->to.reg`), but Pconv's printer omits it
     * when it happens to already equal the destination, the same
     * shape of omission as the AJAL/REGLINK bug found earlier in this
     * session. No goken reference exists to byte-verify this specific
     * construct against either way (real `ia` rejects it), so this is
     * implemented by direct analogy rather than hand-decoded bytes --
     * unlike every other gap this session, which was always verified
     * against real encoded output before being trusted. *)
    | ArithF ((op, prec), (FR fs1), None, (FR fd)) ->
        rules is_64 env init_data
          { node with instr = T.I (ArithF ((op, prec), (FR fs1), Some (FR fd), (FR fd))) }

    | ArithF ((op, prec), (FR fs1), Some (FR fs2), (FR fd)) ->
        let funct7 = (match op, prec with
          | ADD_, A.F -> 0x00 | ADD_, A.D -> 0x01
          | SUB_, A.F -> 0x04 | SUB_, A.D -> 0x05
          | MUL_, A.F -> 0x08 | MUL_, A.D -> 0x09
          (* claude: FDIV.S/D -- same "low bit selects precision"
           * pattern as ADD/SUB/MUL above (confirmed against real
           * RISC-V's own F-extension encoding, standard 0x0c/0x0d,
           * not separately byte-verified against goken since it's
           * the same op_rftype shape as the 3 already-verified ops
           * above with only funct7 changing). Found stress-testing
           * real lib_core/libc (fmt/fltfmt.c's own real "DIVD
           * F2,F30,F0"). *)
          | DIV_, A.F -> 0x0c | DIV_, A.D -> 0x0d
          | (ABS_ | NEG_), _ ->
              failwith "Codegeni: ArithF: ABS_/NEG_ not wired, \
                        no real closure needs them yet"
        ) in
        { size = 4; x = None; binary = (fun () -> [ op_rftype funct7 fs1 7 fs2 fd ]) }

    (* claude: "CMPEQD Fa,Fb,Rd"/"CMPLTD"/"CMPLED" -- real RISC-V
     * FEQ/FLT/FLE.S/D, funct7=0x50 (float)/0x51 (double), funct3/rm
     * selects which of the 3 (0=FLE,1=FLT,2=FEQ, standard RISC-V
     * encoding) -- confirmed against real `linkers/il/optab.c`'s own
     * "ACMPEQF/ACMPLTF/ACMPLEF/ACMPEQD/ACMPLTD/ACMPLED, C_FREG,C_REG
     * -> OOP_FP" rows, and the funct3 values themselves by
     * hand-decoding all 3 real double forms directly (goken's own
     * optab.c param columns aren't self-evidently "which funct3" on
     * their own). Same `from`->rs2 / `middle`->rs1 / `to`->rd mapping
     * as `ArithF` above for all 3 (confirmed by hand-decoding
     * "CMPEQD/CMPLTD/CMPLED F28,F0,Rd": real rs1=F0(middle),
     * rs2=F28(from) in every case). *)
    | CmpF ((op, A.F), (FR fs1), (FR fs2), (R rd)) ->
        let funct3 = (match op with EQ_ -> 2 | LT_ -> 1 | LE_ -> 0) in
        { size = 4; x = None; binary = (fun () -> [ op_rftype 0x50 fs1 funct3 fs2 rd ]) }
    | CmpF ((op, A.D), (FR fs1), (FR fs2), (R rd)) ->
        let funct3 = (match op with EQ_ -> 2 | LT_ -> 1 | LE_ -> 0) in
        { size = 4; x = None; binary = (fun () -> [ op_rftype 0x51 fs1 funct3 fs2 rd ]) }

    (* --------------------------------------------------------------------- *)
    (* System *)
    (* --------------------------------------------------------------------- *)

    (* case 24:		/* SYS *)
    | ECALL ->
        { size = 4; x = None; binary = (fun () ->
          [ op_itype op_system 0 rZERO rZERO 0 ]
        )}

    (* case 22:	/* CSRRx C, rs, rd */ *)
    | CSR (op, csrnum, rs, rd) ->
        let funct3 = (match op with CSRRW -> 1 | CSRRS -> 2 | CSRRC -> 3) in
        { size = 4; x = None; binary = (fun () ->
          [ op_itype op_system funct3 rs rd csrnum ]
        )}

    (* --------------------------------------------------------------------- *)
    (* Control flow *)
    (* --------------------------------------------------------------------- *)

    (* claude: RISC-V has no branch-delay slot (unlike MIPS) -- see
     * docs/claude_notes/riscv_port.md /
     * docs/claude_notes/mips_port.md for that story. RET (Rewritei.ml)
     * expands to a plain
     * `JMP (RLINK)`, encoded as `JALR x0, 0(RLINK)` (rd=x0 means
     * "don't save a return address", i.e. an unconditional jump).
     *)
    | JMP { contents = (IndirectJump rt) } ->
        { size = 4; x = None; binary = (fun () ->
          [ op_itype 0x67 (* JALR opcode *) 0 rt rZERO 0 ]
        )}

    (* case 4:	/* jal [D,]L */ *)
    (* claude: unconditional jump-to-label (JMP, no link -- goken's C
     * defaults `r` to REGZERO here, and JMP's own grammar production
     * never allows an explicit override) and jump-and-link-to-label
     * with an explicit link register (JALR here despite the
     * confusing name clash with goken's separate JALR/case-5
     * indirect-through-register form -- see the AST comment on
     * Ast_asmi.ml's JALR: xix's own grammar produces JALR, not JAL,
     * for "JAL reg,label" syntax, reusing the same constructor as
     * the genuinely-indirect "JALR reg,(reg2)" form; this arm
     * exactly targets the label-shaped Absolute case, the other
     * (IndirectJump, an actual nonzero-rd computed jump) is a
     * follow-up -- see docs/claude_notes/riscv_port.md).
     * Bare `JAL` (Ast_asmi's own JAL constructor, defaulting r to
     * REGLINK, no explicit register at all) is also wired here for
     * completeness, even though it can't be verified directly
     * against goken -- goken's own "JAL" mnemonic grammar production
     * *always* requires an explicit register (`LCALL sreg ',' rel`),
     * so there's no matching goken source syntax to diff against;
     * it shares the exact same encoding path as the tested
     * JALR-with-Absolute arm below, just with a different r. *)
    (* claude: the `fits_jal_range` check happens *inside* the lazy
     * `binary` thunk, not eagerly here -- `node.real_pc`/the branch
     * target's own `real_pc` aren't finalized yet during the sizing
     * pass (`size_of_instruction` calls `rules` once before layout
     * has run just to get instruction sizes), so computing
     * `branch_delta` eagerly here would check a meaningless
     * not-yet-resolved delta. Caught this the hard way: it broke an
     * existing, previously-passing fixture (jal_case4.s) that has
     * nothing to do with an actually-out-of-range branch at all. *)
    | JMP { contents = (Absolute _) } ->
        { size = 4; x = None; binary = (fun () ->
          let delta = branch_delta node in
          if not (fits_jal_range delta)
          then error node "TODO: case 18 (far JMP, LUI+JALR fallback) not implemented"
          else [ op_jtype rZERO delta ]
        )}
    | JAL { contents = (Absolute _) } ->
        { size = 4; x = None; binary = (fun () ->
          let delta = branch_delta node in
          if not (fits_jal_range delta)
          then error node "TODO: case 18 (far JAL, LUI+JALR fallback) not implemented"
          else [ op_jtype rLINK delta ]
        )}
    | JALR (rd, { contents = (Absolute _) }) ->
        { size = 4; x = None; binary = (fun () ->
          let delta = branch_delta node in
          if not (fits_jal_range delta)
          then error node "TODO: case 18 (far JALR, LUI+JALR fallback) not implemented"
          else [ op_jtype rd delta ]
          )}

    (* case 5: jalr D,I(S) / jmp I(S) -- indirect jump through a
     * register plus a signed 12-bit offset (goken's `OP_I(classreg
     * (to), r, v)`); generalizes the RET-only `JMP (IndirectJump _)`
     * arm above (offset always 0, rd always REGZERO there) to an
     * arbitrary offset and rd, now reachable directly from a .s file
     * via "JMP I(S)"/"JALR D,I(S)" -- see Ast_asmi.ml's JALRI comment. *)
    | JALRI (rd, rs, offset) ->
        if not (fits_addi_imm offset)
        then error node "TODO: jalr offset out of 12-bit range"
        else
          { size = 4; x = None; binary = (fun () ->
            [ op_itype 0x67 (* JALR opcode *) 0 rs rd offset ]
          )}

    (* case 3:	/* beq S,[R,]L */ *)
    (* claude: goken's own grammar (a.y) assigns operands
     * asymmetrically between the 1- and 2-register forms, *not* a
     * simple "middle defaults to zero" like every other case this
     * session: "BEQ R1,R2,L" (2 explicit regs) puts from=R1,
     * reg(middle)=R2, matching case 3's `OP_B(r=middle, from, v)`
     * formula directly (rs1=R2,rs2=R1) -- but "BEQ R1,L" (1 reg)
     * is REWRITTEN BY THE GRAMMAR ITSELF into from=$zero,reg=R1
     * (`outcode($1, &regzero, $2.reg, &$4)` in a.y), landing R1 in
     * the *middle* slot, not `from` -- so it encodes as rs1=R1,
     * rs2=$zero (branch if R1 == 0), not rs1=0,rs2=R1 as a naive
     * "default the omitted operand to zero" reading of case 3's C
     * would suggest. Confirmed by decoding goken's actual output
     * bytes for both forms directly (not just eyeballing `il -a`'s
     * pretty-printed text, which turned out ambiguous about
     * operand order). *)
    | Bxx (cond, GReg rf, middle, { contents = (Absolute _) }) ->
        let (rs1, rs2) = (match middle with
          | Some rm -> (rm, rf)
          | None -> (rf, rZERO)
        ) in
        (* claude: GT/LE have no direct hardware encoding -- goken's
         * real linker (linkers/il/obj.c, case ABGT/ABGTU/ABLE/ABLEU)
         * rewrites them into the reversed-relation LT/GE form, with
         * both the condition *and* the two register roles reversed
         * ("BLEU Ra,Rb,L" => hardware "BGEU" encoded exactly as
         * source-level "BGEU Rb,Ra,L" would be) -- confirmed by
         * decoding real ia/il's own output bytes directly (an earlier
         * version of this fix assumed only the condition needed
         * remapping, reusing `rs1`/`rs2` as computed above unchanged;
         * that produced rs1/rs2 reversed from real goken's own bytes,
         * caught immediately by comparing raw B-type instruction
         * words, not just trusting the derivation). *)
        let (cond, rs1, rs2) = match cond with
          | GT sign -> (LT sign, rs2, rs1)
          | LE sign -> (GE sign, rs2, rs1)
          | c -> (c, rs1, rs2)
        in
        let funct3 = opirr_bxx_funct3 cond in
        { size = 4; x = None; binary = (fun () ->
          [ op_btype funct3 rs1 rs2 (branch_delta node) ]
        )}

    (* --------------------------------------------------------------------- *)
    (* Memory / Address *)
    (* --------------------------------------------------------------------- *)

    (* case 2:		/* addi $I,[R,]D */ ("MOVW $imm,R"; the implicit
     * middle here is RZERO, not the destination, since this is the
     * MOV-immediate pseudo-op, not a general Arith -- same asymmetry
     * as ARM's Codegen5.ml (MOV/MVN default to r=0, other ops
     * default to r=destination); see also case 9 for imm too big to
     * fit in 12 bits.
     *)
    (* claude: shared with V__ ("MOV $imm,R") -- both real AMOVW and
     * AMOV immediate loads produce the exact same "addi"/"lui,addi"
     * bytes (confirmed against real ia/il; unlike the reg-to-reg
     * case just below, which is genuinely asymmetric between the two
     * mnemonics -- there's no second real register here for that
     * asymmetry to apply to, just RZERO either way). *)
    | Move2 ((W__ | V__), Right (Int i), Gen (GReg rt)) ->
        if fits_addi_imm i
        then
          { size = 4; x = None; binary = (fun () ->
            [ op_itype op_opimm 0 rZERO rt i ]
          )}
        else if i land 0xfff = 0
        then
          (* case 8:	/* lui	I,D */
           * claude: goken's own constant classifier (il/span.c's
           * `aclass`) picks this 4-byte LUI-only encoding over
           * case 9's LUI+ADDI specifically when the immediate's low
           * 12 bits are exactly zero (its C_UCON class) -- no ADDI
           * is needed since there'd be nothing to add. Missing this
           * fast path (i.e. always taking case 9 below) would still
           * be functionally correct but silently 4 bytes too big
           * whenever this triggers, so byte-identical only by
           * accident for the (far more common) non-aligned case. *)
          { size = 4; x = None; binary = (fun () -> [ op_utype op_lui rt i ]) }
        else
          (* case 9:	/* lui I1,D; addi I0,D */ *)
          { size = 8; x = None; binary = (fun () -> gen_absolute rt i) }

    | Move2 (W__, Right (Float _), Gen (GReg _)) ->
        failwith "TODO: ?? because of refactor of imm_or_ximm"
    | Move2 (W__, Right (String _), Gen (GReg _)) ->
        (* stricter? what does il do with that? confusing I think *)
        error node "string not allowed in MOVW; use DATA"

    (* claude: shared with V__ ("MOV $sym(SB),R") -- same reasoning as
     * the Int-immediate case above: this is an ADDI/LUI+ADDI address
     * computation with only one real register operand (RSB), so
     * there's no second-register slot for the MOV-vs-MOVW asymmetry
     * (confirmed by the reg-to-reg case's own comment) to apply to. *)
    | Move2 ((W__ | V__), Right (Address (Global (global, goffset))), Gen (GReg rt)) ->
        (* claude: `env.syms`'s own TEXT entries (`T.SText2`) are
         * added *incrementally*, one at a time, as Layouti.layout_text
         * walks the code graph forward (a symbol's own real_pc isn't
         * knowable until every instruction before it has already been
         * sized) -- confirmed by reading Layouti.ml's own single
         * forward `T.iter` loop, which calls this very function
         * (`size_of_instruction`) and only *afterwards* registers the
         * TEXT symbol for the node just visited. DATA symbols
         * (`T.SData2`), by contrast, are all registered up front by
         * `Layout.layout_data`, which runs *before* layout_text even
         * starts (confirmed in CLI.ml's own linking pipeline order) --
         * so a lookup miss here can only mean one of two things: a
         * genuinely undefined symbol, or a real TEXT symbol that just
         * hasn't been visited yet (a *forward* reference -- "MOV
         * $later_func(SB),R" appearing textually before that
         * function's own TEXT block, found stress-testing real
         * lib_core/libc's fmt/fmtfd.c, whose own fmtfdinit takes the
         * address of fmt/fmtfdflush.c's __fmtFdFlush, defined in a
         * *later* unit of the very same link). Since a TEXT-symbol
         * address-of always takes the absolute-load path regardless
         * of its value (`size=8` unconditionally, same as the
         * resolved case just below), the SIZE doesn't actually need
         * the lookup to succeed at all here -- only the real *value*
         * does, and that can wait for the binary thunk, evaluated
         * later during the real codegen pass (by which point
         * layout_text has finished and every TEXT symbol, including
         * this one, is registered) -- same deferred-to-the-thunk
         * pattern already used by the SData2 slow-path's own
         * `init_data` lookup just below. A miss THERE (inside the
         * thunk) is a genuine undefined-symbol error, not deferred
         * further.
         *
         * claude: a real, confirmed bug found stress-testing real
         * lib_core/libc (fmt/dofmt.c's own real "%d" digit-table
         * setup, "MOV $.string<>+12(SB),R13" -- picking out the
         * "0123456789..." table from partway into a larger, shared
         * `.string<>` data blob): this case's own `global`'s offset
         * field used to be named `_offsetTODO` and was never added
         * into ANY of the 3 address formulas below -- every
         * "$sym+N(SB)" address-of with a nonzero N silently computed
         * the address of "$sym+0(SB)" instead, discarding N entirely.
         * Caught by running the actual linked hello_libc binary under
         * qemu and getting real but WRONG output ("hello from libc.a:
         * i + i = >" instead of "2 + 2 = 4") -- byte-identical
         * differential testing against goken can't catch this class
         * of bug at all, since goken can't even assemble this file's
         * own -S output in the first place (see this file's own
         * running "-S print artifact" comments) -- only a real
         * end-to-end run exposes it. Reproduced minimally with a
         * hand-written multi-DATA `.string<>` blob and confirmed the
         * exact same wrong byte comes back regardless of which
         * nonzero offset is requested (always reads from +0). *)
        (match (try Some (Hashtbl.find env.syms (T.symbol_of_global global)) with
               Not_found -> None) with
        | None ->
            { size = 8; x = None; binary = (fun () ->
              match Hashtbl.find_opt env.syms (T.symbol_of_global global) with
              | Some (T.SText2 real_pc) -> gen_absolute rt (real_pc + goffset)
              | Some (T.SData2 _) ->
                  raise (Impossible "a symbol that was undefined during \
                    layout can't have become a DATA symbol by codegen \
                    time -- all DATA symbols are registered before \
                    layout_text ever starts")
              | None ->
                  error node (spf "undefined: %s" (A.s_of_global global))
            )}
        | Some v ->
        (match v with
        | T.SText2 real_pc ->
            (* address of a procedure: always the absolute-load path,
             * same as ARM/MIPS (a TEXT symbol isn't RSB-relative) *)
            { size = 8; x = None; binary = (fun () -> gen_absolute rt (real_pc + goffset)) }
        | T.SData2 (offset, _kind) ->
            let final_offset = offset_to_SB (offset + goffset) in
            (* case 11:	/* addi $I,R,D */
             * super important condition! for bootstrapping setSB in
             * MOVW $setSB(SB), RSB and not transform it into
             * ADDI RSB, RSB, offset_to_SB (circular: RSB isn't set
             * up yet at that point). *)
            if final_offset <> 0 && fits_addi_imm final_offset
            then
              { size = 4; x = None; binary = (fun () ->
                [ op_itype op_opimm 0 rSB rt final_offset ]
              )}
            else
              (* case 20:	/* lui/auipc I1,D; addi I0; D */
               * absolute address = data-segment offset + INITDAT,
               * same formula ARM/MIPS's lcon fallback uses.
               * init_data isn't known yet during the sizing pass
               * (Layouti.layout_text calls size_of_instruction with
               * init_data=None -- only the *size* is needed then,
               * so the actual lookup must stay inside the binary
               * thunk, evaluated only during the later real gen
               * pass), same as Codegen.default_rules's WORD case.
               *)
              { size = 8; x = None; binary = (fun () ->
                match init_data with
                | None -> raise (Impossible "init_data should be set by now")
                | Some init_data ->
                    let target_abs = offset + goffset + init_data in
                    if is_64
                    then
                      (* claude: riscv64/ojl: `vv = regoff(&p->from) +
                       * instoffx - (pc + INITTEXT)` in il/asm.c's
                       * case 20 -- AUIPC encodes a delta from *this
                       * instruction's own* absolute pc, not the
                       * absolute address itself. Unlike goken's raw
                       * `pc` (text-relative, 0 at the first
                       * instruction, needing the explicit +INITTEXT),
                       * xix's node.real_pc is already absolute (see
                       * Layouti.layout_text: `pc := ref init_text`),
                       * so no extra +INITTEXT term is needed here. *)
                      let delta = target_abs - node.real_pc in
                      gen_pcrelative rt delta
                    else gen_absolute rt target_abs
              )}
        ))

    (* claude: "MOV $fmt+0(FP),R9" -- address of a Local/Param
     * pseudo-frame entity, as opposed to `Gen (Entity (Param|Local))`
     * (a memory ACCESS through it, already normalized away to a real
     * `Indirect` by `resolve_entities` above) -- this is computing
     * the address itself as a VALUE, the exact same "ADDI rd,base,
     * offset" shape as case 2's own ADD-immediate (mirroring how the
     * SB-relative address-of-global case just above reduces to an
     * RSB-relative ADDI/LUI+ADDI): reuse `resolve_entity` to get the
     * already-correct `Indirect(rSP, offset)`, then emit exactly what
     * `Arith(ADD None, Imm offset, Some rSP, rt)` would. Found
     * stress-testing real lib_core/libc (fmt/fmt.c's own real "MOV
     * $fmt+0(FP),R9", taking the address of a local `Fmt` struct to
     * pass to a helper by pointer). *)
    | Move2 ((W__ | V__), Right (Address ((A.Param _ | A.Local _) as e)), Gen (GReg rt)) ->
        (match resolve_entity is_64 env e with
        | Indirect (rbase, offset) ->
            if fits_addi_imm offset
            then
              { size = 4; x = None; binary = (fun () ->
                [ op_itype op_opimm 0 rbase rt offset ]
              )}
            else
              { size = 12; x = None; binary = (fun () ->
                let (lui_bits, low12) = gen_upper_and_low_via op_lui rTMP offset in
                [ lui_bits; op_itype op_opimm 0 rTMP rTMP low12;
                  op_rtype op_op 0 0 rbase rTMP rt ]
              )}
        | GReg _ | Entity _ ->
            raise (Impossible "resolve_entity always returns Indirect for Param/Local")
        )

    (* case 6:		/* sb R,I(S) */
     * claude: generalized to a shared helper -- goken's case 6 is
     * fully generic over SB/SH/SW/SD, the only difference between
     * mnemonics being which funct3 the optab looked up beforehand;
     * this mirrors that by taking funct3 as a parameter, used below
     * both by Move2's W__ ("MOVW", always 32-bit, confirmed against
     * goken's own optab.c: AMOVW's row is unconditional, never
     * is_64-gated) and Move1's B_/H_ (byte/half stores, e.g.
     * user-written "MOVB R,I(S)"). V__ ("MOV", bare/pointer-width --
     * needed by Rewritei.ml's own link-register save/restore, always
     * a full pointer, 8 bytes on riscv64 -- confirmed empirically
     * that bare "MOV" is genuinely is_64-dependent in goken, unlike
     * "MOVW") gets its own, separate arm just below rather than
     * sharing W__'s is_64-independent funct3 -- conflating the two
     * was a real, confirmed bug (silently truncating RLINK's save on
     * riscv64), caught while porting case 15/16. *)
    | Move2 (W__, Left (Gen (GReg rf)), Gen (Indirect (rbase, offset))) ->
        gen_store node 2 (* SW, always *) rbase rf offset
    | Move2 (V__, Left (Gen (GReg rf)), Gen (Indirect (rbase, offset))) ->
        gen_store node (if is_64 then 3 (* SD *) else 2 (* SW *)) rbase rf offset

    (* claude: "MOVW $0,off(Rbase)" -- storing a bare immediate
     * directly to memory. Real RISC-V's S-type store has no immediate
     * operand slot at all (rs2 is always a register field), so this
     * can never be one real instruction as literally written --
     * confirmed real goken's own `ia` REJECTS this exact syntax
     * outright ("syntax error"), even though it's genuine -S output
     * from a real closure (found in several files real goken already
     * can't reassemble its own output for, e.g. fmt/dofmt.c). Root
     * cause: goken's own optab.c has no "immediate -> memory" store
     * row at all, only "AMOVW, C_ZREG, C_SOREG -> OSTORE" (a
     * REGISTER classified as C_ZREG, i.e. REGZERO) -- a 5th instance
     * of this whole effort's running "-S print artifact" bug family
     * (compilers/ic/list.c's Pconv apparently renders a from-operand
     * that's really `D_REG,REGZERO` as "$0" instead of "R0" when the
     * value happens to be the constant 0, indistinguishable in VALUE
     * but not in real ia's own grammar). Since there's no goken
     * reference to byte-match against for these particular files
     * anyway (goken can't assemble them either), fixed at this port's
     * own assembler/codegen level instead of goken's C source: treat
     * a zero-valued immediate store exactly as if RZERO had been
     * written, which is unambiguously the objectively correct real
     * encoding either way. Only $0 is handled -- a genuinely nonzero
     * immediate store would need real materialization into a scratch
     * register first (an `ADD $imm,RZERO,RTMP` ahead of the store),
     * not yet implemented since no real closure needs it. *)
    | Move2 (W__, Right (Int 0), Gen (Indirect (rbase, offset))) ->
        gen_store node 2 (* SW, always *) rbase rZERO offset

    (* claude: plain register-to-register move -- real RISC-V has no
     * dedicated MOV opcode, spelled as the "ADD rd,x0,rs" idiom (the
     * always-zero x0 register plus the source register). A previous
     * version of this comment claimed "MOVW"/"MOV" (W__/V__) put the
     * source register in a DIFFERENT operand slot from each other
     * (rs1=source for MOVW vs rs1=x0 for MOV) -- that was a testing
     * artifact: the fixture used to confirm it was "MOVW R0,R8",
     * whose *source* happens to be x0 itself, making rs1-vs-rs2 order
     * genuinely indistinguishable by construction (x0 in either slot
     * produces identical bytes). Re-verified with a non-zero source
     * ("MOVW R8,R10") and found real goken's own bytes actually match
     * "MOV"'s own convention exactly (rs1=x0, rs2=source, same as
     * V__ below) -- so W__ and V__ share the identical encoding here,
     * unlike the case 6/7/15/16 memory-access shapes just above
     * (whose W__-vs-V__ split is real and independently confirmed:
     * MOVW is always 32-bit while a bare MOV is pointer-width). Found
     * stress-testing real lib_core/libc (fmt/dofmt.c's own real
     * "MOVW R8,R14"/"MOVW R14,R8" register moves). *)
    | Move2 ((W__ | V__), Left (Gen (GReg rf)), Gen (GReg rt)) ->
        { size = 4; x = None; binary = (fun () -> [ op_rtype op_op 0 0 rZERO rf rt ]) }

    | Move1 (B_ _, Left (GReg rf), Indirect (rbase, offset)) ->
        gen_store node 0 (* SB *) rbase rf offset
    | Move1 (H_ _, Left (GReg rf), Indirect (rbase, offset)) ->
        gen_store node 1 (* SH *) rbase rf offset

    (* claude: "MOVB $0,off(Rbase)" -- same "immediate zero store"
     * -S-print artifact as the MOVW $0 case above (goken's own
     * optab.c: "AMOVB, C_ZREG, C_SOREG -> OSTORE" -- a register
     * classified C_ZREG, not a real immediate-to-memory store; real
     * `ia` has no such single instruction to encode this literally).
     * Same fix: treat the zero immediate as RZERO. Found
     * stress-testing real lib_core/libc (fmt/dofmt.c's own real
     * "MOVB $0,0(R8)"). *)
    | Move1 (B_ _, Right (Int 0), Indirect (rbase, offset)) ->
        gen_store node 0 (* SB *) rbase rZERO offset
    | Move1 (H_ _, Right (Int 0), Indirect (rbase, offset)) ->
        gen_store node 1 (* SH *) rbase rZERO offset

    (* claude: FSW/FSD -- same S-type shape as SW/SD above (goken's
     * own optab.c: "fsw"/"fsd", C_FREG,C_SOREG -> OSTORE, same funct3
     * convention as the load side's FLW/FLD, just STORE-FP's own
     * major opcode 0x27 instead of LOAD-FP's 0x07). `gen_store`
     * doesn't care which register *file* its `rf` belongs to (the
     * bit-field encoding is identical either way) -- unwrap/rewrap
     * the `freg` as a bare int through the same `R` constructor
     * `op_stype` already expects, purely for that field. Found
     * stress-testing real lib_core/libc (fmt/strtod.c's own real
     * "MOVD F0,x-8(SP)"). *)
    | Move2 (F__, Left (GFReg (FR rf)), Gen (Indirect (rbase, offset))) ->
        gen_store ~opcode:0x27 node 2 (* FSW *) rbase (R rf) offset
    | Move2 (D__, Left (GFReg (FR rf)), Gen (Indirect (rbase, offset))) ->
        gen_store ~opcode:0x27 node 3 (* FSD *) rbase (R rf) offset

    (* case 7:		/* lb I(S),D */
     * claude: same generalization as case 6 -- goken picks the
     * funct3 (0/4=LB/LBU, 1/5=LH/LHU, 2=LW, 3=LD) purely from which
     * mnemonic was used, the encoding itself (OP_I) is identical.
     * W__/V__ split matches the store side's own comment above. *)
    | Move2 (W__, Left (Gen (Indirect (rbase, offset))), Gen (GReg rt)) ->
        gen_load node 2 (* LW, always *) rbase rt offset
    | Move2 (V__, Left (Gen (Indirect (rbase, offset))), Gen (GReg rt)) ->
        gen_load node (if is_64 then 3 (* LD *) else 2 (* LW *)) rbase rt offset

    (* claude: FLW/FLD -- mirror of FSW/FSD above, LOAD-FP's own major
     * opcode 0x07 (vs the integer LOAD opcode 0x03). *)
    | Move2 (F__, Left (Gen (Indirect (rbase, offset))), GFReg (FR rt)) ->
        gen_load ~opcode:0x07 node 2 (* FLW *) rbase (R rt) offset
    | Move2 (D__, Left (Gen (Indirect (rbase, offset))), GFReg (FR rt)) ->
        gen_load ~opcode:0x07 node 3 (* FLD *) rbase (R rt) offset

    | Move1 (B_ A.S, Left (Indirect (rbase, offset)), GReg rt) ->
        gen_load node 0 (* LB *) rbase rt offset
    | Move1 (B_ A.U, Left (Indirect (rbase, offset)), GReg rt) ->
        gen_load node 4 (* LBU *) rbase rt offset
    | Move1 (H_ A.S, Left (Indirect (rbase, offset)), GReg rt) ->
        gen_load node 1 (* LH *) rbase rt offset
    | Move1 (H_ A.U, Left (Indirect (rbase, offset)), GReg rt) ->
        gen_load node 5 (* LHU *) rbase rt offset

    (* case 6 (SB-relative fast path, reusing case 6/11's shared
     * "fits addi" test) / case 12 (SB-relative slow path, "mov
     * r,lext"): "MOVW R,sym(SB)" -- store the value of a register to
     * a global, as opposed to Right(Address ...) above which computes
     * the global's *address*. goken's own assembler resolves the
     * small-offset case straight to case 6 (RSB is just another
     * register once biased by BIG, confirmed via optab.c: the same
     * C_SOREG class both a plain "sb R,I(S)" and a small-offset
     * "sb R,sym(SB)" resolve to), only reaching case 12 when the
     * resolved offset doesn't fit ADDI's 12-bit field. *)
    | Move2 (W__, Left (Gen (GReg rf)), Gen (Entity (A.Global (global, goffset)))) ->
        let v = Hashtbl.find env.syms (T.symbol_of_global global) in
        (match v with
        | T.SText2 _ -> error node "TODO: storing to a TEXT symbol"
        | T.SData2 (offset, _kind) ->
            let final_offset = offset_to_SB (offset + goffset) in
            (* claude: unlike case 6/7's own shared helper above (whose
             * `Move2 (W__, ...)` arm is_64-branches because it's also
             * reused internally by Rewritei.ml's own pointer-width
             * RLINK save, never reachable from real .s source), a
             * user-written "MOVW" reaching *this* case is a genuine,
             * direct AMOVW mnemonic -- confirmed against goken's own
             * optab.c: `AMOVW,...,OSTORE,2` is unconditional, not
             * is_64-gated, since the "W" suffix itself already means
             * "32-bit", on either arch. *)
            let funct3 = 2 (* SW, always -- see comment above *) in
            (* claude: no "final_offset <> 0" exclusion here, unlike
             * case 11's address-of -- that exclusion exists solely to
             * avoid a circular ADDI when *defining* setSB itself (an
             * address computation), which doesn't apply to an
             * ordinary store/load memory access. *)
            if fits_addi_imm final_offset
            then gen_store node funct3 rSB rf final_offset
            else
              { size = 8; x = None; binary = (fun () ->
                match init_data with
                | None -> raise (Impossible "init_data should be set by now")
                | Some init_data ->
                    let target_abs = offset + goffset + init_data in
                    if is_64 then
                      let delta = target_abs - node.real_pc in
                      let (lui_bits, low12) = gen_upper_and_low_via op_auipc rTMP delta in
                      [ lui_bits; op_stype 0x23 funct3 rTMP rf low12 ]
                    else
                      let (lui_bits, low12) = gen_upper_and_low_via op_lui rTMP target_abs in
                      [ lui_bits; op_stype 0x23 funct3 rTMP rf low12 ]
              )}
        )

    (* case 7 (SB-relative fast path) / case 13 (SB-relative slow
     * path, "mov lext,r"): "MOVW sym(SB),R" -- load the value at a
     * global, mirror of the store case above. *)
    | Move2 (W__, Left (Gen (Entity (A.Global (global, goffset)))), Gen (GReg rt)) ->
        let v = Hashtbl.find env.syms (T.symbol_of_global global) in
        (match v with
        | T.SText2 _ -> error node "TODO: loading the value at a TEXT symbol"
        | T.SData2 (offset, _kind) ->
            let final_offset = offset_to_SB (offset + goffset) in
            (* claude: always LW (funct3=2) -- see the mirror-image
             * store arm's own comment above. *)
            let funct3 = 2 in
            if fits_addi_imm final_offset
            then gen_load node funct3 rSB rt final_offset
            else
              { size = 8; x = None; binary = (fun () ->
                match init_data with
                | None -> raise (Impossible "init_data should be set by now")
                | Some init_data ->
                    let target_abs = offset + goffset + init_data in
                    if is_64 then
                      let delta = target_abs - node.real_pc in
                      let (lui_bits, low12) = gen_upper_and_low_via op_auipc rTMP delta in
                      [ lui_bits; op_itype 0x03 funct3 rTMP rt low12 ]
                    else
                      let (lui_bits, low12) = gen_upper_and_low_via op_lui rTMP target_abs in
                      [ lui_bits; op_itype 0x03 funct3 rTMP rt low12 ]
              )}
        )

    (* claude: SB-relative float load -- "MOVD sym(SB),F2" (e.g. real
     * fmt/strtod.c's own "MOVD pows10<>+1272(SB),F2", reading a table
     * entry from a real global). Mirror of the W__ Entity-load case
     * just above, LOAD-FP opcode 0x07 instead of LOAD's 0x03; no
     * store counterpart wired (no real closure stress-tested so far
     * writes a float *to* a global, only reads tables like this one). *)
    | Move2 ((F__ | D__) as sz, Left (Gen (Entity (A.Global (global, goffset)))), GFReg (FR rt)) ->
        let v = Hashtbl.find env.syms (T.symbol_of_global global) in
        (match v with
        | T.SText2 _ -> error node "TODO: loading the value at a TEXT symbol"
        | T.SData2 (offset, _kind) ->
            let final_offset = offset_to_SB (offset + goffset) in
            let funct3 = (match sz with F__ -> 2 (* FLW *) | D__ -> 3 (* FLD *)
                          | W__ | V__ -> raise (Impossible "sz restricted to F__|D__ above")) in
            if fits_addi_imm final_offset
            then gen_load ~opcode:0x07 node funct3 rSB (R rt) final_offset
            else
              { size = 8; x = None; binary = (fun () ->
                match init_data with
                | None -> raise (Impossible "init_data should be set by now")
                | Some init_data ->
                    let target_abs = offset + goffset + init_data in
                    if is_64 then
                      let delta = target_abs - node.real_pc in
                      let (lui_bits, low12) = gen_upper_and_low_via op_auipc rTMP delta in
                      [ lui_bits; op_itype 0x07 funct3 rTMP (R rt) low12 ]
                    else
                      let (lui_bits, low12) = gen_upper_and_low_via op_lui rTMP target_abs in
                      [ lui_bits; op_itype 0x07 funct3 rTMP (R rt) low12 ]
              )}
        )

    (* case 10:		/* sign extend */ *)
    (* claude: register-to-register sign/zero-extend, e.g. "MOVB
     * R1,R2" widening a byte already sitting in a register (as
     * opposed to case 6/7's *memory* byte/half access above --
     * disambiguated by the `from` gen being GReg here, Indirect
     * there). goken's asm.c case 10 special-cases MOVBU as a single
     * ANDI $0xFF (why: idiomatic/simpler than a shift pair for the
     * zero-extend-byte case specifically -- goken doesn't do this
     * for MOVHU, which goes through the general shift-pair path
     * below with no arithmetic/sign-extend bit set). All other
     * combinations (MOVB/MOVH/MOVHU) go through SLLI shift-left-then
     * SRLI/SRAI shift-right-back, sign-extending through the top
     * bits when the right shift is arithmetic (SRAI); riscv64/ojl
     * widens the shift amount by 32 since the trick needs to punt
     * the byte/half up to the *top* of a 64-bit register, not a
     * 32-bit one. *)
    | Move1 (B_ A.U, Left (GReg rf), GReg rt) ->
        { size = 4; x = None; binary = (fun () ->
          [ op_itype op_opimm 7 (* ANDI *) rf rt 0xFF ]
        )}
    | Move1 (((B_ A.S | H_ _) as sz), Left (GReg rf), GReg rt) ->
        let shift = (match sz with B_ _ -> 24 | H_ _ -> 16 | W_ _ | V_ _ -> assert false) in
        let shift = if is_64 then shift + 32 else shift in
        let arith_bit = (match sz with B_ A.S | H_ A.S -> 0x20 lsl 5 | _ -> 0) in
        { size = 8; x = None; binary = (fun () ->
          [ op_itype op_opimm 1 (* SLLI *) rf rt (shift land 0x3f);
            op_itype op_opimm 5 (* SRLI/SRAI *) rt rt (shift lor arith_bit);
          ]
        )}

    (* --------------------------------------------------------------------- *)
    (* Other: not ported yet *)
    (* --------------------------------------------------------------------- *)
    | Arith _
    | Move1 _ | Move2 _
    | JMP _ | JAL _ | JALR _ | Bxx _
    | FENCE_I | BREAK | SYS
       ->
       failwith (spf "Codegeni: TODO: instr not handled: %s"
                (Typesi.show_instr node.instr))
    )

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

(* must return a multiple of 4 *)
let size_of_instruction (env : Codegen.env) (node : 'a T.node) : int =
  (* is_64 doesn't affect any instruction's *size* (AUIPC vs LUI is
   * still one 4-byte instruction either way), only the `binary`
   * thunk's contents -- never forced during sizing, so this dummy
   * value is never actually read. *)
  let action = rules false env None node in
  action.size

let gen (symbols2 : T.symbol_table2) (config : Exec_file.linker_config)
   (cg : 'a T.code_graph) : T.word list =

  let res = ref [] in
  let autosize = ref 0 in

  let pc = ref config.init_text in

  cg |> T.iter (fun n ->

    let is_64 = (match config.arch with Arch.Riscv64 -> true | _ -> false) in
    let {size; binary; x = _} =
        rules is_64
        Codegen.{ syms = symbols2; autosize = !autosize }
        config.init_data n
    in
    let instrs = binary () in

    if n.real_pc <> !pc
    then raise (Impossible "Phase error, layout inconsistent with codegen");
    if List.length instrs * 4 <> size
    then raise (Impossible (spf "size of rule does not match #instrs at %s"
                              (T.s_of_loc n.n_loc)));

    let xs : Bits.int32 list = instrs |> List.map Assoc.sort_by_val_highfirst in

    if !Flags.debug_gen
    then begin
      Logs.app (fun m -> m " %.8x: %s (%s)"
                 !pc
                  (xs |> List.map (fun x -> spf "%.8x" (int_of_bits n x))
                      |> String.concat " ")
                  (Str.global_replace (Str.regexp "[\n\t ]+") " "
                     (Typesi.show_instr n.instr) |> String_.show_max 40));
      xs |> List.iter (fun x ->
        let w = int_of_bits n x in
        Logs.debug (fun m -> m "%s (0x%x)" (Dumper.dump x) w);
      );
    end;

    let xs = xs |> List.map (fun x -> int_of_bits n x) in
    res |> Stack_.push xs;

    pc := !pc + size;
    (match n.instr with
    | T.TEXT (_, _, size) -> autosize := size;
    | _ -> ()
    );
  );

  !res |> List.rev |> List.flatten
