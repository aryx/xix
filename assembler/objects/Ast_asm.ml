(* Copyright 2025 Yoann Padioleau, see copyright.txt *)
open Common

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Types common to the different Plan 9 assembler ASTs *)

(*****************************************************************************)
(*  AST related types *)
(*****************************************************************************)

(* ------------------------------------------------------------------------- *)
(* Location *)
(* ------------------------------------------------------------------------- *)

(* (global) line# *)
type loc = int (* same than Location_cpp.loc (repeated here for clarity) *)
[@@deriving show]

(* ------------------------------------------------------------------------- *)
(* Numbers and Strings *)
(* ------------------------------------------------------------------------- *)
(* increments by unit of 1 *)
type virt_pc = int
[@@deriving show]

(* 'int' enough for ARM 32 bits? on 64 bits machine it is enough :) 
 * TODO: use Int64.t so sure it's enough for every arch.
 * alt: have separate type in each Ast_asmxxx.ml, with more precise size
 * but not worth it because it prevents more generalization in this file and
 * anyway Plan 9 asms are not a direct match of the machine assembly.
*)
type integer = int 
[@@deriving show]
(* can be 0, negative, or positive *)
type offset = int
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

(* ------------------------------------------------------------------------- *)
(* Operands *)
(* ------------------------------------------------------------------------- *)

type register = R of int (* between 0 and 15 on ARM *)
[@@deriving show]

type entity = 
  | Param of symbol option * offset (* FP *)
  | Local of symbol option * offset (* SP *)
  (* stricter: we disallow anonymous offsets to SB *)
  | Global of global * offset (* SB *) 
[@@deriving show]

(* extended immediate *)
type ximm =
  | String of string (* limited to 8 characters *)

  (* Float? *)

  (* I used to disallow address of FP or SP, and offset to SB, but
   * 5c needs this feature, so you can take the address of a local.
   * old: Address of global.
   *)
  | Address of entity
[@@deriving show]

(* I use a ref below so the code that resolves branches is shorter.
 * The ref is modified by the assembler and then by the linker.
 *)  
type branch_operand = branch_operand2 ref
and branch_operand2 =

  (* resolved by assembler *)
  (* relative to PC, in units of virtual_code_address *)
  | Relative of int 
  (* we could transform labels in symbols early-on, but nice to resolve ASAP *)
  | LabelUse of label * offset (* useful to have offset? *)

  (* resolved by linker *)
  | SymbolJump of global (* no offset (it would not be used by 5l anyway) *)

  (* after resolution *)
  | Absolute of virt_pc

  (* resolved dynamically by the machine (e.g., B (R14)) *)
  | IndirectJump of register
[@@deriving show]

(* ------------------------------------------------------------------------- *)
(* Instructions *)
(* ------------------------------------------------------------------------- *)

type pseudo_instr =
  (* stricter: we allow only SB for TEXT and GLOBL, and no offset *)
  | TEXT of global * attributes * int (* size locals, should be multiple of 4 *)
  | GLOBL of global (* can have offset? *) * attributes * int (* size *)

  | DATA of global * offset * int (* size, should be in [1..8] *) * imm_or_ximm
  (* any ximm? even String? And Float? for float should have DWORD? *)
  | WORD of imm_or_ximm

  and attributes = { dupok: bool; prof: bool }
  and imm_or_ximm = (integer, ximm) Either_.t
[@@deriving show]

(* ------------------------------------------------------------------------- *)
(* Program *)
(* ------------------------------------------------------------------------- *)

type 'instr line = 
  | Pseudo of pseudo_instr
  | Instr of 'instr

  (* disappear after resolve *)
  | LabelDef of label
  (* less: PragmaLibDirective of string *)
[@@deriving show]

type 'instr program = ('instr line * loc) list
[@@deriving show]
