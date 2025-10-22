(* Copyright 2025 Yoann Padioleau, see copyright.txt *)
open Common

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Types common to the different Plan 9 assembler ASTs
 *
 * Note that in Plan 9 object files are mostly the serialized form of 
 * the assembly AST, which is why this file is in this directory.
 *) 

(*****************************************************************************)
(*  AST related types *)
(*****************************************************************************)

(* ------------------------------------------------------------------------- *)
(* Location *)
(* ------------------------------------------------------------------------- *)

(* (global) line# *)
type loc = int (* Location_cpp.loc *)
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
 * but not worth it because it prevents more generalization in this file 
 * like imm_or_ximm, which in turn allow to generalize pseudo_instr,
 * and anyway Plan 9 asms are not a direct match of the machine assembly.
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

type fregister = F of int (* between 0 and 15 on ARM *)
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

   (* In a MOVE, sign is relevant only for a load operation *)
type sign = Signed | Unsigned
[@@deriving show]
(* TODO: add Dword  *)
type move_size = Word | HalfWord of sign | Byte of sign
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

(* alt: move in arch-specific Ast_asmx.instr
 * alt: merge with pseudo_instr
*)
type virtual_instr =
  | RET
  | NOP (* removed by linker *)
  (* TODO? out MOV here with sizes and sign/unsigned *)
[@@deriving show]

(* ------------------------------------------------------------------------- *)
(* Program *)
(* ------------------------------------------------------------------------- *)

type 'instr line = 
  | Pseudo of pseudo_instr
  | Virtual of virtual_instr
  | Instr of 'instr

  (* disappear after resolve *)
  | LabelDef of label
  (* less: PragmaLibDirective of string *)
[@@deriving show]

(* TODO: and add also Location_cpp.location_history list, like in
 * Object5.ml, because need the loc_hist to convert a loc to the right place
 *)
type 'instr program = ('instr line * loc) list
[@@deriving show]

(*****************************************************************************)
(*  Extractors/visitors *)
(*****************************************************************************)

let s_of_global (x : global) : string = 
  x.name ^ (match x.priv with None -> "" | Some _ -> "<>")

(* Visit globals (e.g., to populate symbol table with wanted symbols in linker).
 * This is more complicated than in 5l because we can not rely
 * on an ANAME or Operand.sym.
 * less: boilerplate which could be auto generated by ocamltarzan in
 *  a visitor_asm.ml
 *)
let rec visit_globals_program visit_instr (f : global -> unit) (xs : 'instr program) : unit =
  xs |> List.iter (fun (x, _line) ->
    match x with
    | Pseudo y ->
      (match y with
      | TEXT (ent, _, _) -> f ent
      | GLOBL (ent, _, _) -> f ent
      | DATA (ent, _, _, ix) -> f ent; visit_globals_imm_or_ximm f ix
      | WORD (ix) -> visit_globals_imm_or_ximm f ix
      )
    | Virtual y ->
      (match y with
      | RET | NOP -> ()
      )
    | Instr instr ->
      visit_instr f instr
    | LabelDef _ -> ()
  )

and visit_globals_ximm f x =
  match x with
  | Address (Global (x, _)) -> f x
  | Address (Param _ | Local _) -> ()
  | String _ -> ()
and visit_globals_imm_or_ximm f x =
  match x with
  | Either.Left _ -> ()
  | Either.Right x -> visit_globals_ximm f x
and visit_globals_branch_operand f x =
  match !x with
  | SymbolJump ent -> f ent
  | IndirectJump _ | Relative _ | LabelUse _ | Absolute _ -> ()
