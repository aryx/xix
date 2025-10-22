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

(* TOPORT *)
type mregister = M of int (* between 0 and 31 *)
[@@deriving show]

(* TOPORT *)
type fcrregister = FCR of int (* between 0 and 31 *)
[@@deriving show]
