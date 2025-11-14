(* Copyright 2025 Yoann Padioleau, see copyright.txt *)
open Common

module A = Ast_asm
open Ast_asm

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Abstract Syntax Tree (AST) for the assembly language supported by ia/ja.
 * I call this language Asmi.
 *
 * Asmi continue the old RISC tradition of fairly simple assembly language.
 * Like in the MIPS there is a LINK register and JAL instruction (and JALR).
 * There is a special instruction LUI to load upper (large) immediate
 * constants and AUIPC for large jmps.
 * Note also the special FENCE instructions for multicore ??
 *
 * In the original doc the registers are named x0 to x31 but certain
 * registers have also alternative and more readable names that
 * illustrate how the should/could be used:
 *  - "special registers": 
 *     * zero (x0) always 0, 
 *     * ra (x1) return address (a.k.a. LINK in MIPS/ARM)
 *     * sp (x2) stack pointer
 *     * gp (x3) global pointer = SB in plan9 philosophy!
 *       so RISCV basically followed what the plan9 guys did for a long time
 *     * tp (x4) thread pointer ???
 *  - a0-a8 are "argument and return registers" and correspond to
 *    x10-x17 (a0 = regarg0/regret0, a1 = regarg1, regret1, a2-a7 args2-7)
 *  - s0-s11 are "saved registers" which are registers that should be
 *    preserved across function calls. If function uses them they should
 *    save/restore them (usually via the stack). They correspond to
 *    x8, x9, x18-x27 (s0 = fp = x8 for saved register / frame pointer,
 *    s1 = x9, s2-s11 = x18-x27 for saved registers)
 *  - t0-t6 for "temporary registers" and correspond to
 *    x5-x7, x28-x31 (t0-t2 = x5-x7, t3-t6 = x28-x31).
 *
 * Note the possiblities to return multiple values.

 * !!! If you modify this file please increment Object_file.version !!!
 *)

(*****************************************************************************)
(* The AST related types *)
(*****************************************************************************)

(* ------------------------------------------------------------------------- *)
(* Operands *)
(* ------------------------------------------------------------------------- *)

type reg = A.register (* between 0 and 31 *)
[@@deriving show]

type freg = A.fregister (* between 0 and 31 *)
[@@deriving show]

(* reserved by the linker *)
let rTMP = R 4 (* supposed to be "thread pointer" in riscv spec *)
let rSP = R 2
let rSB = R 3
(* reserved by hardware *)
let rLINK = R 1

(* always contain the value 0 *)
let rZERO = R 0

let nb_registers = 32
let nb_fregisters = 32


(* alt: could call it arith_operand but use imr like in the original grammar *)
type imr =
  | Imm of A.integer
  | IReg of reg
[@@deriving show {with_path = false}]

(* alt: could call move_operand1 but follow naming of original grammar *)
type gen =
  | GReg of reg
  | Indirect of reg * A.offset
  | Entity of A.entity
[@@deriving show {with_path = false}]

(* alt: move_operand2 *)
type vgen =
  | Gen of gen
  (* | ... far more stuff *)
[@@deriving show {with_path = false}]

(* ------------------------------------------------------------------------- *)
(* Instructions *)
(* ------------------------------------------------------------------------- *)
type instr =
  (* Arithmetic *)
  | Arith of arith_opcode * imr * reg option * reg
  | NOR of imr * reg option * imr
  | ArithMul of mul_opcode * reg * reg option * reg
  (* TODO: in theory takes F | D | W, not just A.floatp_precision *)
  | ArithF of (arithf_opcode * A.floatp_precision) *
       freg * freg option * freg

  (* Memory (Load/Store) *)
  (* "one side must be a register" *)
  | Move1 of move1_size * (gen, ximm) Either_.t * gen
  (* "one side must be a register" *)
  | Move2 of move2_size * (vgen, ximm) Either_.t * vgen

  (* Control flow *)
  | JMP of A.branch_operand
  | RFE of A.branch_operand
  | JAL of A.branch_operand (* jump and link *)
  | JALReg of reg * A.branch_operand (* no Relative|LabelUse here *) 
  (* "left side must be register" *)
  | BEQ of gen * reg option * A.branch_operand (* just Relative|LabelUse *)
  | BNE of gen * reg option * A.branch_operand (* just Relative|LabelUse *)
  | Bxx of b_condition * gen * A.branch_operand (* just Relative|LabelUse *)

  (* System *)
  | SYSCALL
  | BREAK
  | TLB of tlb_kind

  and arith_opcode =
    (* logic *)
    | AND | OR | XOR
    (* arithmetic *)
    | ADD of size * A.sign  | SUB of size * A.sign (* converted to ADD(-) in vl *)
    (* bitshifting *)
    | SLL of size | SRA of size | SRL of size
    (* less useful *)
    | SGT of A.sign

  and size = W (* word, 32 bits *) | V (* vlong, 64 bits *)
  and mul_opcode =
    (* TODO? in va/a.h there is REMVU/REMV but not in va/lex.c, weird *)
    | MUL of size * A.sign | DIV of size * A.sign | REM of A.sign

  (* ABS/NEG are unary and can't take middle register. Same for CMPFxx 
   * alt: define separate ArithFUnary CmpF constructs
   *)
  and arithf_opcode =
    | ADD_ | SUB_ | DIV_ | MUL_
    | ABS_ | NEG_
    | CMPEQ_ | CMPGE_ | CMPGT_

  and move1_size = 
     | B_ (* Byte *) of A.sign
     | H_ (* Half world *) of A.sign
     | W_ (* Word *) of move_dir
     | V_ (* Very long *) of move_dir
  and move_dir = Le (* Left *) | Ri (* Right *)

  and move2_size =
    | W__ (* Word *)  | V__ (* Very long *)
    | F__ (* Float *) | D__ (* Double *)

  and b_condition =
    (* Great/Less Equal Zero (AL = ?) *)
    | GEZ | GEZAL | GTZ | LEZ | LTZ | LTZAL

  and tlb_kind =
    (* ?? *)
    | P_ | R_ | WI | WR

[@@deriving show {with_path = false}]

(* ------------------------------------------------------------------------- *)
(* Program *)
(* ------------------------------------------------------------------------- *)

(* for ocaml-light to work without deriving *)
let show_program _ = "NO DERIVING"
[@@warning "-32"]

type program = instr A.program
[@@deriving show]

(*****************************************************************************)
(* Extractors/Visitors *)
(*****************************************************************************)
let branch_opd_of_instr (instr: instr) : A.branch_operand option =
  match instr with
  (* ocaml-light: could factorize more (JMP opd | RFE opd | ...) -> Some opd *)
  | JMP opd -> Some opd
  | RFE opd -> Some opd
  | JAL opd -> Some opd
  | JALReg (_, opd) -> Some opd
  | BEQ (_, _, opd) -> Some opd
  | BNE (_, _, opd) -> Some opd
  | Bxx (_, _, opd) -> Some opd
  | Arith _ | ArithF _ | NOR _ | ArithMul _ | Move1 _ | Move2 _ | SYSCALL | BREAK
  | TLB _ -> None

let visit_globals_instr (f : global -> unit) (i : instr) : unit =
  let mov_operand x =
    match x with
    | Entity (A.Global (x, _)) -> f x
    | Entity (A.Param _ | A.Local _) -> ()
    | GReg _ | Indirect _ -> ()
  in
  let mov_vgen x =
    match x with
    | Gen x -> mov_operand x
  in
  match i with
  | Move1 (_, x1, gen2) -> 
      (match x1 with
      | Either.Left gen1 -> mov_operand gen1
      | Either.Right ximm1 -> A.visit_globals_ximm f ximm1
      );
      mov_operand gen2
  | Move2 (_, x1, vgen2) -> 
      (match x1 with
      | Either.Left vgen1 -> mov_vgen vgen1
      | Either.Right ximm1 -> A.visit_globals_ximm f ximm1
      );
      mov_vgen vgen2

  | JMP b -> A.visit_globals_branch_operand f b
  | RFE b -> A.visit_globals_branch_operand f b
  | JAL b -> A.visit_globals_branch_operand f b
  | JALReg (_, b) -> A.visit_globals_branch_operand f b
  | BEQ (gen, _, b) ->
      mov_operand gen;
      A.visit_globals_branch_operand f b
  | BNE (gen, _, b) ->
      mov_operand gen;
      A.visit_globals_branch_operand f b
  | Bxx (_, gen, b) ->
      mov_operand gen;
      A.visit_globals_branch_operand f b
  | Arith _ | NOR _ | ArithMul _ | ArithF _ 
  | SYSCALL | BREAK | TLB _ -> () 
