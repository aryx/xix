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
(* Abstract Syntax Tree (AST) for the assembly language supported by 7a/7l
 * (goken's Plan 9 ARM64/AArch64 assembler/linker). I call this language
 * Asm7. See docs/claude_notes/notes_arm64_port_plan.txt for the overall
 * port plan and how this compares to the ARM32/MIPS/RISC-V ports already
 * done.
 *
 * claude: register 31 is contextual in real AArch64: goken's own grammar
 * (assemblers/7a/a.y) lexes "ZR" and "RSP" to the *same* D_REG/reg=31 node
 * (lex.c: both map to plain LREG, value 31) -- it's the *encoding
 * position* an instruction puts register 31 in that decides whether the
 * CPU reads it as the zero register or the stack pointer, not any tag in
 * the AST. The bare "SP" keyword (without "R"/"Z" prefix) is a genuinely
 * different goken token (LSP) producing a separate D_SP node when used as
 * a plain register operand -- but this port never spells it that way
 * (bare "SP" as a *register operand* isn't wired in the grammar below;
 * write "RSP" or "ZR" instead, both settling on `R 31` here, exactly
 * mirroring goken's own two-spellings-one-encoding choice). This is
 * unrelated to xix's own, pre-existing TSP/TFP tokens (shared across
 * every arch, Ast_asm.entity's Local/Param) which spell Plan9-style
 * local/parameter-relative addressing ("x(SP)"/"x(FP)"), not the
 * physical stack-pointer register at all.
 *
 * claude: unlike ARM32 (where every instruction carries a per-instruction
 * condition-code suffix, `instr_with_cond = instr * condition`),
 * AArch64 dropped that feature almost entirely -- confirmed in a.y:
 * conditional execution only appears on branches (as distinct BEQ/BNE/...
 * mnemonics, not a generic ".cond" suffix) and on the CSEL/CSET family
 * (where `cond` is a real, explicit grammar *operand*, e.g. "CSET
 * EQ,R1"). So `instr` here is a flat type with no condition wrapper, the
 * same shape as Ast_asmi.ml (RISC-V), not Ast_asm5.ml (ARM32).
 *
 * claude: goken's own address-of-global / large-constant mechanism for
 * ARM64 turned out (confirmed empirically against goken's actual 7a/7l,
 * not guessed from reading asmout.c alone) to be structurally the *same*
 * two-tier "fast SB-relative path, literal-pool slow path" shape as
 * ARM32/MIPS/RISC-V, not the ADRP+MOVK sequence one might expect from
 * real-world AArch64 codegen: REGSB (X28) is set up to point at the very
 * start of the data segment (goken's `pass.c`: `xdefine("setSB", SDATA,
 * 0L)` -- offset 0, i.e. *no* BIG-style bias at all, unlike ARM32's
 * BIG=4092 or RISC-V's BIG=2048), and a small-enough SB-relative offset
 * becomes a single scaled-immediate LDR/STR (confirmed: "MOV foo(SB),R1"
 * assembles to plain `ldr x1, [x28]` when foo is at data-offset 0).
 * Anything that doesn't fit falls back to goken's own literal pool
 * (`omovlit()` in asmout.c, using PC-relative "LDR (literal)" to read a
 * nearby pool word, addpool()/flushpool()-style, same family as
 * Layout5.ml's mechanism) -- see Layout7.ml/Codegen7.ml.
 *
 * Scope for this first version (see notes_arm64_port_plan.txt's "Suggested
 * phase plan" for what's deliberately deferred): SIMD/vector registers,
 * atomics, system instructions (MRS/MSR/SYS/DMB/HINT), the conditional-
 * select family (CSEL/CSET/CINC/...), bitfield move/extract as their own
 * mnemonics (BFM/BFI/EXTR -- LSL/LSR/ASR/ROR *by immediate* still work,
 * see `Shift` below, since goken implements those via the same bitfield
 * encoding family but they're common enough to be worth including from
 * the start), floating point, load/store *pair* (LDP/STP), and the
 * extended-register / pre-post-increment addressing modes are all left
 * for a follow-up phase -- see the port log once one exists for what's
 * actually landed.
 *)

(*****************************************************************************)
(* The AST related types *)
(*****************************************************************************)

(* ------------------------------------------------------------------------- *)
(* Operands *)
(* ------------------------------------------------------------------------- *)

type reg = A.register (* between 0 and 31; 31 is ZR or RSP depending on
                        * context, see this file's prelude comment *)
[@@deriving show]

type freg = A.fregister (* SIMD/FP register, between 0 and 31; the
                          * precision (S/D/Q, i.e. 32/64/128-bit view) is
                          * selected by the mnemonic, same convention as
                          * ARM32's freg + floatp_precision *)
[@@deriving show]

(* reserved by the linker (include/objexec/7.out.h's REG* constants) *)
let rTMP  = R 17 (* REGIP1, goken's REGTMP *)
let rSB   = R 28 (* REGSB -- see this file's prelude comment: no BIG bias *)
let rFP   = R 29 (* REGFP, goken's frame-pointer register *)
let rLINK = R 30 (* REGLINK *)
let rZERO = R 31 (* REGZERO -- spelled "ZR" (or "RSP" when meant as the
                   * stack pointer instead); see prelude comment *)
let rSP   = R 31

(* used by the compiler for calling conventions *)
let rRET = R 0

let nb_registers = 32
let nb_fregisters = 32

(* claude: goken's `imsr` (imm | shift | extreg) narrowed to imm | reg for
 * now -- extreg (UXTB/UXTH/UXTW/UXTX/SXTB/SXTH/SXTW/SXTX-extended
 * register operands, used for pointer arithmetic combining a 32-bit index
 * register into a 64-bit computation) is a real AArch64-only addressing
 * mode, deferred -- see this file's prelude comment. *)
type imr =
  | Imm of A.integer
  | Reg of reg
[@@deriving show { with_path = false }]

type gen =
  | GReg of reg
  | Indirect of reg * A.offset
  | Entity of A.entity
  (* claude: goken's D_XPRE/D_XPOST -- pre/post-index writeback
   * addressing ("-16(RSP)!" / "(RSP)16!"), needed for Rewrite7.ml's
   * RETURN-expansion link-register save/restore (a real, reachable
   * addressing mode, not just an RFE-style internal-only construct --
   * see PreIndex/PostIndex's own grammar comment in Parser_asm7.mly). *)
  | PreIndex of reg * A.offset
  | PostIndex of reg * A.offset
[@@deriving show { with_path = false }]

(* ------------------------------------------------------------------------- *)
(* Instructions *)
(* ------------------------------------------------------------------------- *)
type instr =
  (* claude: LTYPE1 in a.y -- ADD/SUB/AND/ORR/EOR/BIC (register-register
   * or register-immediate, goken's `imsr` -- immediate here means the
   * "addcon" shape: a 12-bit unsigned value, optionally shifted left by
   * 12, see gen_addcon in Codegen7.ml) and MUL (register-only; goken's
   * case 15 also covers MADD/MNEG/UMULH/UMULL/accumulate forms, deferred
   * -- see ArithMul below for the one MUL-family shape wired so far). *)
  | Arith of arith_opcode * imr * reg option * reg
  (* claude: goken's case 8 (shift by immediate, via UBFM/SBFM/EXTR --
   * genuinely different encoding family from Arith above, even though
   * "LSL $c,[R,]R" reads like an ordinary 3-operand arith op) and case 9
   * (shift by register, via LSLV/LSRV/ASRV/RORV -- same simple oprrr
   * shape as Arith's register form). Kept as its own constructor rather
   * than folded into Arith since the *immediate* form's encoding has
   * nothing in common with Arith's. *)
  | Shift of shift_opcode * imr * reg option * reg
  (* claude: LTYPE7 -- CMP/CMN, no destination register (implicitly ZR);
   * same imr as Arith. *)
  | Cmp of cmp_opcode * imr * reg
  (* claude: LTYPE15/case 15's simple 3-operand MUL (goken's `MUL
   * Rm,[Rn,]Rd` -- accumulate-register forms (MADD/MSUB with an explicit
   * 4th operand, and the *W-suffixed 32-bit-result variants) are a
   * follow-up -- see ArithMul's "not wired" comment in Codegen7.ml. *)
  | ArithMul of reg * reg option * reg
  (* claude: LTYPE3 in a.y -- MOV/MOVB/MOVBU/MOVH/MOVHU/MOVW/MOVWU, all
   * one grammar shape ("gen,gen") dispatched by operand type at codegen
   * time, exactly like Ast_asmi.ml's Move1/RISC-V and unlike ARM32's
   * split MOVE/SWAP-per-size-class. The register-immediate form ("MOV
   * $con,R") goes through the same SB-relative-fast-path/literal-pool
   * mechanism as address-of-global -- see this file's prelude comment. *)
  | Move of move_size * (gen, A.ximm) Either_.t * gen

  (* Control flow *)
  (* claude: LTYPE4 -- unconditional branch (B) / branch-and-link (BL),
   * either to a label or indirectly through a register (goken's `nireg`
   * alternative, "B (R1)"/"BL (R1)"). *)
  | B of A.branch_operand
  | BL of A.branch_operand
  (* claude: LTYPE5 -- BEQ/BNE/.../BLE, label-only (goken's `rel`, no
   * indirect-through-register form for conditional branches). *)
  | Bxx of condition * A.branch_operand
  (* claude: LTYPE8 -- CBZ/CBNZ Rt,label ("compare and branch if
   * (non)zero"). *)
  | CBxx of bool (* true = branch if nonzero (CBNZ), false = CBZ *) *
      reg * A.branch_operand
  (* claude: LTYPEA -- RET[reg], defaults to RLINK (X30) when the
   * register is omitted, matching goken's own default. Unlike
   * ARM32/MIPS/RISC-V (where "RET" is purely a compiler-facing virtual
   * instr, expanded by Rewrite{5,v,i}.ml since 5a/va/ia have no real RET
   * mnemonic at all), AArch64 genuinely has RET as its own hardware
   * instruction/mnemonic -- so this is a real, directly-parseable instr
   * here, not routed through the shared Ast_asm.virtual_instr.RET at
   * all. *)
  | RET of reg option

  (* System *)
  (* claude: LTYPE6 -- SVC (goken's case 10 also covers BRK/HVC/HLT/
   * DCPS1-3/DRPS/CLREX with the same shape, deferred: SVC is the only
   * one any Linux userspace program needs). *)
  | SVC of int

  and arith_opcode =
    | ADD | SUB | AND_ | ORR | EOR | BIC
  and shift_opcode =
    | LSL | LSR | ASR | ROR
  and cmp_opcode =
    | CMP | CMN

  and move_size =
    | B_ of A.sign (* byte *)
    | H_ of A.sign (* halfword *)
    | W_ of A.sign (* word (32-bit); S = sign-extend to 64 on load (MOVW),
                    * U = zero-extend (MOVWU) *)
    | X_ (* doubleword (64-bit), bare "MOV" *)

  (* claude: same shape/encoding as Ast_asm5.condition (ARM32) -- AArch64
   * reused the identical 4-bit condition-code encoding (0=EQ..14=AL,
   * 15=NV), just with a couple of extra mnemonic synonyms in the
   * grammar (BCS/BHS both mean GE U, BCC/BLO both mean LT U -- see
   * Parse_asm7.ml). *)
  and condition =
    | EQ | NE
    | GT of A.sign | LT of A.sign | GE of A.sign | LE of A.sign
    | MI | PL | VS | VC
    | AL

[@@deriving show { with_path = false }]

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
  | B opd -> Some opd
  | BL opd -> Some opd
  | Bxx (_, opd) -> Some opd
  | CBxx (_, _, opd) -> Some opd
  | Arith _ | Shift _ | Cmp _ | ArithMul _ | Move _ | RET _ | SVC _ -> None

let visit_globals_instr (f : global -> unit) (i : instr) : unit =
  let mov_operand x =
    match x with
    | Entity (A.Global (x, _)) -> f x
    | Entity (A.Param _ | A.Local _) -> ()
    | GReg _ | Indirect _ | PreIndex _ | PostIndex _ -> ()
  in
  match i with
  | Move (_, x1, gen2) ->
      (match x1 with
      | Either.Left gen1 -> mov_operand gen1
      | Either.Right ximm1 -> A.visit_globals_ximm f ximm1
      );
      mov_operand gen2
  | B b -> A.visit_globals_branch_operand f b
  | BL b -> A.visit_globals_branch_operand f b
  | Bxx (_, b) -> A.visit_globals_branch_operand f b
  | CBxx (_, _, b) -> A.visit_globals_branch_operand f b
  | Arith _ | Shift _ | Cmp _ | ArithMul _ | RET _ | SVC _ -> ()
