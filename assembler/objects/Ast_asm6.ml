(* Copyright 2025 Yoann Padioleau, see copyright.txt *)
open Common

module A = Ast_asm
open Ast_asm

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Abstract Syntax Tree (AST) for the assembly language supported by 6a.
 * I call this language Asm6.
 *)

(*****************************************************************************)
(* The AST related types *)
(*****************************************************************************)

(* ------------------------------------------------------------------------- *)
(* Operands *)
(* ------------------------------------------------------------------------- *)

type register = A.register (* between 8 and 15 *)
[@@deriving show]

type fregister = A.fregister (* between 0 and 7 *)
[@@deriving show]

type mregister = M of int (* between 0 and 7 *)
[@@deriving show]

type xregister = X of int (* between 0 and 15 *)
[@@deriving show]

type crregister = CR of int (* between 0 and 15 *)
[@@deriving show]

type drregister = DR of int (* between 0 and 7 *)
[@@deriving show]

type trregister = TR of int (* between 0 and 7 *)
[@@deriving show]
