(* Copyright 2025 Yoann Padioleau, see copyright.txt *)
open Common

module A = Ast_asm
open Ast_asm

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Abstract Syntax Tree (AST) for the assembly language supported by va.
 * I call this language Asmv.
 *)

(*****************************************************************************)
(* The AST related types *)
(*****************************************************************************)

(* ------------------------------------------------------------------------- *)
(* Operands *)
(* ------------------------------------------------------------------------- *)

type register = A.register (* between 0 and 31 *)
[@@deriving show]

type fregister = A.fregister (* between 0 and 31 *)
[@@deriving show]

(* ?? *)
type mregister = M of int (* between 0 and 31 *)
[@@deriving show]

(* ?? *)
type fcrregister = FCR of int (* between 0 and 31 *)
[@@deriving show]

(* reserved by the linker *)
let rTMP = R 28
let rSP = R 29
let rSB = R 30
(* reserved by hardware *)
let rLINK = R 31

let nb_registers = 32


(* alt: arith_operand *)
type imr =
  | Imm of A.integer
  | Reg of register
[@@deriving show]

(* alt: move_operand1 *)
type gen =
  | GReg of register
  | Indirect of register * A.offset
  | Entity of A.entity
[@@deriving show]

(* alt: move_operand2 *)
type vgen =
  | Gen of gen
  (* | ... far more stuff *)
[@@deriving show]

(* ------------------------------------------------------------------------- *)
(* Instructions *)
(* ------------------------------------------------------------------------- *)
type instr =
  (* Arithmetic *)
  | Arith of arith_opcode * imr * register option * register
  | NOR of imr * register option * imr
  | ArithMul of mul_opcode * register * register option * register

  (* Memory *)
  (* "one side must be a register" *)
  | MoveG of moveg_size * (gen, ximm) Either_.t * gen
  (* "one side must be a register" *)
  | MoveV of (vgen, ximm) Either_.t * vgen

  (* Control flow *)
  | JMP of A.branch_operand
  | RFE of A.branch_operand
  | JAL of A.branch_operand (* jump and link *)
  | JALReg of register * A.branch_operand (* no Relative|LabelUse here *) 
  (* "left side must be register" *)
  | BEQ of gen * register option * A.branch_operand (* just Relative|LabelUse *)
  | BNE of gen * register option * A.branch_operand (* just Relative|LabelUse *)
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

  and moveg_size = 
     | B_ (* Byte *) of A.sign
     | H_ (* Half world *) of A.sign
     | W_ (* word *) of move_dir
     | V_ (* very long *) of move_dir
  and move_dir = Le (* Left *) | Ri (* Right *)

  and b_condition =
    | GEZ | GEZAL | GTZ | LEZ | LTZ | LTZAL

  and tlb_kind =
    (* ?? *)
    | P | R | WI | WR

[@@deriving show]

(* ------------------------------------------------------------------------- *)
(* Program *)
(* ------------------------------------------------------------------------- *)

type program = instr A.program
[@@deriving show]

(*****************************************************************************)
(* Extractors/Visitors *)
(*****************************************************************************)
let branch_opd_of_instr (_instr: instr) : A.branch_operand option =
  failwith "TODO:branch_opd_of_instr"
