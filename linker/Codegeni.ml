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
 * docs/claude_notes/notes_riscv_port_plan.txt.
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
 * 0x23), shared by SB/SH/SW/SD (see op_itype's LOAD counterpart). *)
let op_stype funct3 (R rs1) (R rs2) (imm : int) : Bits.t =
  [(0x23, 0); (imm land 0x1f, 7); (funct3, 12);
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
let gen_store (_node : 'a T.node) (funct3 : int) (rbase : reg) (rf : reg) (offset : int) =
  if fits_addi_imm offset
  then { size = 4; x = None; binary = (fun () -> [ op_stype funct3 rbase rf offset ]) }
  else
    { size = 12; x = None; binary = (fun () ->
      let (lui_bits, low12) = gen_upper_and_low_via op_lui rTMP offset in
      [ lui_bits; op_rtype op_op 0 0 rbase rTMP rTMP; op_stype funct3 rTMP rf low12 ]
    )}
let gen_load (_node : 'a T.node) (funct3 : int) (rbase : reg) (rt : reg) (offset : int) =
  if fits_addi_imm offset
  then { size = 4; x = None; binary = (fun () -> [ op_itype 0x03 funct3 rbase rt offset ]) }
  else
    { size = 12; x = None; binary = (fun () ->
      let (lui_bits, low12) = gen_upper_and_low_via op_lui rTMP offset in
      [ lui_bits; op_rtype op_op 0 0 rbase rTMP rTMP; op_itype 0x03 funct3 rTMP rt low12 ]
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
 * have no direct hardware encoding at all -- goken's assembler
 * doesn't accept "BLE"/"BGT" mnemonics either (real RISC-V has no
 * such instructions; they'd need an operand-swapping pseudo-op
 * rewrite, e.g. "BLE a,b,L" => "BGE b,a,L", which isn't wired in
 * the grammar -- see Parse_asmi.ml). *)
let opirr_bxx_funct3 (c : b_condition) : int =
  match c with
  | EQ -> 0
  | NE -> 1
  | LT A.S -> 4
  | GE A.S -> 5
  | LT A.U -> 6
  | GE A.U -> 7
  | GT _ | LE _ -> failwith "TODO:opirr_bxx_funct3 GT/LE (no direct RISC-V encoding)"

(* claude: RISC-V has no branch-delay slot (unlike MIPS -- see
 * notes_riscv_port_plan.txt), so a branch/jump's immediate is just a
 * plain PC-relative delta, no -4/-8 bias to account for. `node.
 * real_pc` and the branch target's `real_pc` are both absolute (not
 * goken's text-relative raw `pc`), but since this is a *difference*
 * the constant INITTEXT offset cancels out either way -- same
 * reasoning already used for case 20's riscv64/AUIPC delta. *)
let branch_delta (node : 'a T.node) : int =
  match node.branch with
  | None -> raise (Impossible "resolving should have set the branch field")
  | Some ndst -> ndst.real_pc - node.real_pc

(*****************************************************************************)
(* The rules! *)
(*****************************************************************************)
(* conventions:
 * - rf = register from (p->from.reg in il)
 * - rt = register to (p->to.reg in il)
 *)

(* claude: is_64 is only ever read inside a `binary` thunk (never
 * during the sizing pass -- see size_of_instruction below), mirroring
 * how init_data is threaded; it's riscv64/ojl's equivalent of goken's
 * global `thechar == 'j'` check. *)
let rules (is_64 : bool)
    (env : Codegen.env) (init_data : T.addr option) (node : 'a T.node) =
  match node.instr with
  (* Reusable *)
  | T.Virt _ | T.TEXT _ | T.WORD _ ->
      Codegen.default_rules env init_data node

  | T.I instr ->
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
     * docs/claude_notes/notes_riscv_port_plan.txt /
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
     * follow-up -- see docs/claude_notes/todo_riscv_port.org).
     * Bare `JAL` (Ast_asmi's own JAL constructor, defaulting r to
     * REGLINK, no explicit register at all) is also wired here for
     * completeness, even though it can't be verified directly
     * against goken -- goken's own "JAL" mnemonic grammar production
     * *always* requires an explicit register (`LCALL sreg ',' rel`),
     * so there's no matching goken source syntax to diff against;
     * it shares the exact same encoding path as the tested
     * JALR-with-Absolute arm below, just with a different r. *)
    | JMP { contents = (Absolute _) } ->
        { size = 4; x = None; binary = (fun () ->
          [ op_jtype rZERO (branch_delta node) ]
        )}
    | JAL { contents = (Absolute _) } ->
        { size = 4; x = None; binary = (fun () ->
          [ op_jtype rLINK (branch_delta node) ]
        )}
    | JALR (rd, { contents = (Absolute _) }) ->
        { size = 4; x = None; binary = (fun () ->
          [ op_jtype rd (branch_delta node) ]
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
    | Move2 (W__, Right (Int i), Gen (GReg rt)) ->
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

    | Move2 (W__, Right (Address (Global (global, _offsetTODO))), Gen (GReg rt)) ->
        let v = Hashtbl.find env.syms (T.symbol_of_global global) in
        (match v with
        | T.SText2 real_pc ->
            (* address of a procedure: always the absolute-load path,
             * same as ARM/MIPS (a TEXT symbol isn't RSB-relative) *)
            { size = 8; x = None; binary = (fun () -> gen_absolute rt real_pc) }
        | T.SData2 (offset, _kind) ->
            let final_offset = offset_to_SB offset in
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
                    let target_abs = offset + init_data in
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

    | Move1 (B_ _, Left (GReg rf), Indirect (rbase, offset)) ->
        gen_store node 0 (* SB *) rbase rf offset
    | Move1 (H_ _, Left (GReg rf), Indirect (rbase, offset)) ->
        gen_store node 1 (* SH *) rbase rf offset

    (* case 7:		/* lb I(S),D */
     * claude: same generalization as case 6 -- goken picks the
     * funct3 (0/4=LB/LBU, 1/5=LH/LHU, 2=LW, 3=LD) purely from which
     * mnemonic was used, the encoding itself (OP_I) is identical.
     * W__/V__ split matches the store side's own comment above. *)
    | Move2 (W__, Left (Gen (Indirect (rbase, offset))), Gen (GReg rt)) ->
        gen_load node 2 (* LW, always *) rbase rt offset
    | Move2 (V__, Left (Gen (Indirect (rbase, offset))), Gen (GReg rt)) ->
        gen_load node (if is_64 then 3 (* LD *) else 2 (* LW *)) rbase rt offset

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
                      [ lui_bits; op_stype funct3 rTMP rf low12 ]
                    else
                      let (lui_bits, low12) = gen_upper_and_low_via op_lui rTMP target_abs in
                      [ lui_bits; op_stype funct3 rTMP rf low12 ]
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
    | Arith _ | ArithMul _ | ArithF _
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
