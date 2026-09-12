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
        let (R r) = (match middle with Some x -> x | None -> rt) in
        if not (fits_addi_imm i)
        then error node "TODO: addi immediate out of 12-bit range"
        else
          { size = 4; x = None; binary = (fun () ->
            [ op_itype op_opimm 0 (R r) rt i ]
          )}

    (* --------------------------------------------------------------------- *)
    (* System *)
    (* --------------------------------------------------------------------- *)

    (* case 24:		/* SYS *)
    | ECALL ->
        { size = 4; x = None; binary = (fun () ->
          [ op_itype op_system 0 rZERO rZERO 0 ]
        )}

    (* --------------------------------------------------------------------- *)
    (* Control flow *)
    (* --------------------------------------------------------------------- *)

    (* claude: RISC-V has no branch-delay slot (unlike MIPS) -- see
     * docs/claude_notes/notes_riscv_port_plan.txt / notes_mips_port_plan.txt
     * for that story. RET (Rewritei.ml) expands to a plain
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

    | Move2 (W__, Left (Gen (GReg rf)), Gen (Indirect (rbase, offset))) ->
        (* case 6:		/* sb R,I(S) */
         * word/doubleword store (funct3=010=SW on riscv32, 011=SD on
         * riscv64 -- RLINK is a full pointer, so its save must widen
         * with the arch; goken's case 6 covers SB/SH/SW/SD generally,
         * keyed off the move's size, but only Word is wired here since
         * that's all Rewritei.ml's link-register-save prologue needs,
         * `MOVW RLINK,0(SP)`; the byte/half variants are a follow-up
         * once move2_size grows B__/H__ constructors -- see
         * Parse_asmi.ml's TODO comment). claude: this previously
         * hardcoded funct3=0 (SB, byte store) despite being reached
         * only for W__ moves -- silently wrong (a byte store instead
         * of a word/doubleword store) until case4's JAL-triggered
         * non-leaf prologue exercised it byte-for-byte against goken
         * for the first time (SW on riscv32; the is_64/SD gap was
         * then caught the same way testing case4 on riscv64). *)
        if not (fits_addi_imm offset)
        then error node "TODO: store offset out of 12-bit range"
        else
          { size = 4; x = None; binary = (fun () ->
            (* S-type: imm[31:25] rs2[24:20] rs1[19:15] funct3[14:12]
             * imm[11:7] opcode[6:0] *)
            let (R rs2) = rf and (R rs1) = rbase in
            let funct3 = if is_64 then 3 (* SD *) else 2 (* SW *) in
            [ [(0x23, 0); (offset land 0x1f, 7); (funct3, 12);
               (rs1, 15); (rs2, 20); ((offset lsr 5) land 0x7f, 25)] ]
          )}

    | Move2 (W__, Left (Gen (Indirect (rbase, offset))), Gen (GReg rt)) ->
        (* case 7:		/* lb I(S),D */
         * load, needed by Rewritei.ml's link-register-restore
         * epilogue (`MOVW 0(SP),RLINK`) -- LW on riscv32, LD on
         * riscv64, same reasoning as case 6's store width. *)
        if not (fits_addi_imm offset)
        then error node "TODO: load offset out of 12-bit range"
        else
          { size = 4; x = None; binary = (fun () ->
            let funct3 = if is_64 then 3 (* LD *) else 2 (* LW *) in
            [ op_itype 0x03 (* LOAD opcode *) funct3
                rbase rt offset ]
          )}

    (* --------------------------------------------------------------------- *)
    (* Other: not ported yet *)
    (* --------------------------------------------------------------------- *)
    | Arith _ | ArithMul _ | ArithF _ | LUI
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
