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

type mregister = M of int (* between 0 and 31 *)
[@@deriving show]

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

(* alt: move_operand *)
type gen =
  | Imr of imr

(* ------------------------------------------------------------------------- *)
(* Instructions *)
(* ------------------------------------------------------------------------- *)
type instr =
  (* Arithmetic *)
  | Arith of arith_opcode * imr * register option * register
  | NOR of imr * register option * imr
  | ArithMul of mul_opcode * register * register option * register

  (* Memory *)
  | Move

  (* Control flow *)
  | JAL
  | JMP
  | Bxx

  (* System *)
  | SYSCALL
  | BREAK
  | RFE
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
