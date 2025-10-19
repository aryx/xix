(* Copyright 2025 Yoann Padioleau, see copyright.txt *)
open Common

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Types common to the different Plan 9 assemblers *)

(*****************************************************************************)
(* The AST related types *)
(*****************************************************************************)

(* (global) line# *)
type loc = int (* same than Location_cpp.loc (repeated here for clarity) *)
[@@deriving show]

(* ------------------------------------------------------------------------- *)
(* Numbers and Strings *)
(* ------------------------------------------------------------------------- *)
(* increments by unit of 1 *)
type virt_pc = int
[@@deriving show]

type label = string
[@@deriving show]
type symbol = string
[@@deriving show]

type global = {
  name: symbol;
  (* 'Some _' when entity is a private symbol (aka static symbol).
   * mutable (ugly?) modifed by linker in naming phase.
   *)
  mutable priv: int option; 
  (* for safe linking (generated only by 5c, not 5a) *)
  signature: int option;
}
[@@deriving show]
