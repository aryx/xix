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

(* ------------------------------------------------------------------------- *)
(* Instructions *)
(* ------------------------------------------------------------------------- *)
type instr =
  (* Arithmetic *)

  (* Memory *)
  | MOV

  (* Control flow *)
  | JAL
  | JMP
  | Bxx

  (* System *)
  | SYSCALL
  | BREAK
  | RFE
  | TBLP

  and arith_opcode = 
    (* logic *)
    | AND | OR | XOR | NOR
    (* arithmetic *)
    | ADD | SUB | MUL | DIV | REM
    (* bitshifting *)
    | SLL | SRA | SRL
    (* less useful *)
    | ABS | NEG

(* ------------------------------------------------------------------------- *)
(* Program *)
(* ------------------------------------------------------------------------- *)

(*****************************************************************************)
(* Extractors/Visitors *)
(*****************************************************************************)
