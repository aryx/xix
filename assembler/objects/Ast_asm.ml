(*s: objects/Ast_asm.ml *)
(* Copyright 2025 Yoann Padioleau, see copyright.txt *)
open Common

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Types common to the different Plan 9 assembler ASTs.
 *
 * Note that in Plan 9 object files are mostly the serialized form of 
 * the assembly AST, which is why this file is in this directory.
 *
 * !!! If you modify this file please increment Object_file.version !!!
 *) 

(*****************************************************************************)
(*  AST related types *)
(*****************************************************************************)

(* ------------------------------------------------------------------------- *)
(* Location *)
(* ------------------------------------------------------------------------- *)
(*s: type [[Ast_asm.loc]] *)
(* (global) line# *)
type loc = Location_cpp.loc
(*e: type [[Ast_asm.loc]] *)
[@@deriving show]

(* ------------------------------------------------------------------------- *)
(* Numbers and Strings *)
(* ------------------------------------------------------------------------- *)
(*s: type [[Ast_asm.virt_pc]] *)
(* increments by unit of 1 *)
type virt_pc = int
(*e: type [[Ast_asm.virt_pc]] *)
[@@deriving show]

(* 'int' enough for ARM 32 bits? on 64 bits machine it is enough :) 
 * TODO: use Int64.t so sure it's enough for every arch.
 * alt: have separate type in each Ast_asmxxx.ml, with more precise size
 * but not worth it because it prevents more generalization in this file 
 * like imm_or_ximm, which in turn allow to generalize pseudo_instr,
 * and anyway Plan 9 asms are not a direct match of the machine assembly.
*)
(*s: type [[Ast_asm.integer]] *)
type integer = int 
(*e: type [[Ast_asm.integer]] *)
[@@deriving show]

(* floating point number.
 * TODO: float enough too? can contain 'double' C? 
 * alt? use explicit float record { Int32.t; Int32.t mantisse/exp } ?
*)
(*s: type [[Ast_asm.floatp]] *)
type floatp = float 
(*e: type [[Ast_asm.floatp]] *)
[@@deriving show]

(*s: type [[Ast_asm.offset]] *)
(* can be 0, negative, or positive *)
type offset = int
(*e: type [[Ast_asm.offset]] *)
[@@deriving show]

(*s: type [[Ast_asm.label]] *)
(* jmp labels *)
type label = string
(*e: type [[Ast_asm.label]] *)
[@@deriving show]

(*s: type [[Ast_asm.symbol]] *)
type symbol = string
(*e: type [[Ast_asm.symbol]] *)
[@@deriving show]

(*s: type [[Ast_asm.global]] *)
type global = {
  name: symbol;
  (*s: [[Asm_asm.global]] other fields *)
  (* 'Some _' when entity is a private symbol (aka static symbol).
   * mutable (ugly?) modifed by linker in naming phase.
   *)
  mutable priv: int option; 
  (*x: [[Asm_asm.global]] other fields *)
  (* for safe linking (generated only by 5c, not 5a) *)
  signature: int option;
  (*e: [[Asm_asm.global]] other fields *)
}
(*e: type [[Ast_asm.global]] *)
[@@deriving show]

(* ------------------------------------------------------------------------- *)
(* Operands *)
(* ------------------------------------------------------------------------- *)
(*s: type [[Ast_asm.register]] *)
type register = R of int (* between 0 and 15 on ARM *)
(*e: type [[Ast_asm.register]] *)
[@@deriving show { with_path = false}]

(*s: type [[Ast_asm.fregister]] *)
type fregister = FR of int (* between 0 and 15 on ARM *)
(*e: type [[Ast_asm.fregister]] *)
[@@deriving show { with_path = false}]

(*s: type [[Ast_asm.entity]] *)
type entity = 
  | Param of symbol option * offset (* FP *)
  | Local of symbol option * offset (* SP *)
  (* stricter: we disallow anonymous offsets to SB *)
  | Global of global * offset (* SB *) 
(*e: type [[Ast_asm.entity]] *)
[@@deriving show { with_path = false}]

(* extended immediate *)
(*s: type [[Ast_asm.ximm]] *)
type ximm =
  | Int of integer
  | String of string (* limited to 8 characters *)
  (*s: [[Ast_asm.ximm]] other cases *)
  (* I used to disallow address of FP or SP, and offset to SB, but
   * 5c needs this feature, so you can take the address of a local.
   * old: Address of global.
   *)
  | Address of entity
  (*x: [[Ast_asm.ximm]] other cases *)
  | Float of floatp
  (*e: [[Ast_asm.ximm]] other cases *)
(*e: type [[Ast_asm.ximm]] *)
[@@deriving show { with_path = false}]

(* I use a ref below so the code that resolves branches is shorter.
 * The ref is modified by the assembler and then by the linker.
 *)  
(*s: type [[Ast_asm.branch_operand]] *)
type branch_operand = branch_operand2 ref
(*e: type [[Ast_asm.branch_operand]] *)
(*s: type [[Ast_asm.branch_operand2]] *)
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
(*e: type [[Ast_asm.branch_operand2]] *)
[@@deriving show { with_path = false}]

(* In a MOVE, sign is relevant only for a load operation *)
(*s: type [[Ast_asm.sign]] *)
type sign = S (* Signed *) | U (* Unsigned *)
(*e: type [[Ast_asm.sign]] *)
[@@deriving show { with_path = false}]

(* TODO: add Dword? use shorter W_ | H_ | B_ | D_ | V_ ?  *)
(*s: type [[Ast_asm.move_size]] *)
type move_size = Word | HalfWord of sign | Byte of sign
(*e: type [[Ast_asm.move_size]] *)
[@@deriving show { with_path = false}]

(*s: type [[Ast_asm.floatp_precision]] *)
type floatp_precision = F (* Float *) | D (* Double *)
(*e: type [[Ast_asm.floatp_precision]] *)
[@@deriving show { with_path = false}]

(* ------------------------------------------------------------------------- *)
(* Instructions *)
(* ------------------------------------------------------------------------- *)

(*s: type [[Ast_asm.pseudo_instr]] *)
type pseudo_instr =
  (* stricter: we allow only SB for TEXT and GLOBL, and no offset *)
  | TEXT of global * attributes * int (* size locals, (multiple of 4 on ARM) *)
  | GLOBL of global (* can have offset? *) * attributes * int (* size *)

  | DATA of global * offset * int (* size, should be in [1..8] *) * ximm
  (* any ximm? even String? And Float? for float should have DWORD? *)
  | WORD of ximm
(*e: type [[Ast_asm.pseudo_instr]] *)

(*s: type [[Ast_asm.attributes]] *)
  and attributes = { dupok: bool; prof: bool }
(*e: type [[Ast_asm.attributes]] *)
[@@deriving show {with_path = false}]

(* alt: move in arch-specific Ast_asmx.instr
 * alt: merge with pseudo_instr
*)
(*s: type [[Ast_asm.virtual_instr]] *)
type virtual_instr =
  (* removed by linker *)
  | RET
  | NOP
  (* new: I introduced those to factorize code in Profile.ml *)
  | Call of global (* transformed in BL in 5l, JAL in vl, etc. *)
  | Load of entity * register
  | Store of register * entity
  | Add of sign * integer (* or ximm? *) * register

(*e: type [[Ast_asm.virtual_instr]] *)
[@@deriving show { with_path = false}]

(* ------------------------------------------------------------------------- *)
(* Program *)
(* ------------------------------------------------------------------------- *)

(*s: type [[Ast_asm.line]] *)
type 'instr line = 
  | Pseudo of pseudo_instr
  | Virtual of virtual_instr
  | Instr of 'instr

  (* disappear after resolve *)
  | LabelDef of label
  (* less: PragmaLibDirective of string *)
(*e: type [[Ast_asm.line]] *)
[@@deriving show {with_path = false}]

(*s: type [[Ast_asm.lines]] *)
type 'instr lines = ('instr line * loc) list
(*e: type [[Ast_asm.lines]] *)
[@@deriving show]

(*s: type [[Ast_asm.program]] *)
(* The location_history allows to convert a loc to the right place *)
type 'instr program = 'instr lines * Location_cpp.location_history list
(*e: type [[Ast_asm.program]] *)
[@@deriving show]

(*****************************************************************************)
(*  Extractors/visitors *)
(*****************************************************************************)

(*s: function [[Ast_asm.s_of_global]] *)
let s_of_global (x : global) : string = 
  x.name ^ (match x.priv with None -> "" | Some _ -> "<>")
(*e: function [[Ast_asm.s_of_global]] *)

(*s: function [[Ast_asm.visit_globals_program]] *)
(* Visit globals (e.g., to populate symbol table with wanted symbols in linker).
 * This is more complicated than in 5l because we can not rely
 * on an ANAME or Operand.sym.
 * less: boilerplate which could be auto generated by ocamltarzan in
 *  a visitor_asm.ml
 *)
let rec visit_globals_program visit_instr (f : global -> unit) (prog : 'instr program) : unit =
  let (xs, _locs) = prog in
  xs |> List.iter (fun (x, _line) ->
    match x with
    | Pseudo y ->
      (match y with
      | TEXT (glob, _, _) -> f glob
      | GLOBL (glob, _, _) -> f glob
      | DATA (glob, _, _, ix) -> f glob; visit_globals_ximm f ix
      | WORD (ix) -> visit_globals_ximm f ix
      )
    | LabelDef _ -> ()
    | Virtual virt ->
          (match virt with
          | RET | NOP -> ()
          | Call glob -> f glob
          | Load (Global (glob, _), _) -> f glob
          | Store (_, Global (glob, _)) -> f glob
          | Load _ | Store _ -> ()
          | Add _ -> ()
          )
    | Instr instr ->
      visit_instr f instr
  )

and visit_globals_ximm f x =
  match x with
  | Address (Global (x, _)) -> f x
  | Address (Param _ | Local _) | String _ | Int _ | Float _ -> ()
and visit_globals_branch_operand f x =
  match !x with
  | SymbolJump ent -> f ent
  | IndirectJump _ | Relative _ | LabelUse _ | Absolute _ -> ()
(*e: function [[Ast_asm.visit_globals_program]] *)
(*e: objects/Ast_asm.ml *)
