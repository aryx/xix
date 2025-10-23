(* Copyright 2015, 2016 Yoann Padioleau, see copyright.txt *)
open Common

module A = Ast_asm
open Ast_asm

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Abstract Syntax Tree (AST) for the assembly language supported by 5a.
 * I call this language Asm5.
 *
 * Note that many types are now defined in Ast_asm.ml instead because they are
 * mostly arch independent and can be reused in other plan9 assemblers
 * (e.g. va, ia, 7a).
 *
 * !!! If you modify this file please increment Object5.version !!!
 * 
 * TODO: 
 *  - floats instructions (or better in Ast_asm.ml?)
 *  - 5c-only opcodes? CASE, BCASE, MULU/DIVU/MODU (or better in Ast_asm.ml too?)
 *  - MULA, MULL,
 *  - MOVM (and his special bits .IA/...), 
 *  - PSR, MCR/MRC,
 *)

(*****************************************************************************)
(* The AST related types *)
(*****************************************************************************)

(* ------------------------------------------------------------------------- *)
(* Numbers and Strings *)
(* ------------------------------------------------------------------------- *)
(* see Ast_asm.ml *)

(* ------------------------------------------------------------------------- *)
(* Operands *)
(* ------------------------------------------------------------------------- *)

type register = A.register (* between 0 and 15 *)
[@@deriving show]

type fregister = A.fregister (* between 0 and 15 *)
[@@deriving show]

type cregister = C of int (* between 0 and 15 *)

(* reserved by linker *)
let rTMP = R 11
let rSB  = R 12
let rSP  = R 13
(* reserved by hardware *)
let rLINK = R 14
let rPC   = R 15

let nb_registers = 16

type arith_operand =
  | Imm of A.integer (* characters are converted to integers *)
  | Reg of register
  (* can not be used with shift opcodes (SLL/SRL/SRA) *)
  | Shift of register * shift_reg_op * 
             (register, int (* between 0 and 31 *)) Either_.t

  and shift_reg_op =
    | Sh_logic_left | Sh_logic_right
    | Sh_arith_right | Sh_rotate_right
[@@deriving show]

(* alt: could almost be moved to Ast_asm.ml but Shift above of arith_operand
 * seems arm-specific
 *)
type mov_operand = 
  (* Immediate shift register *)
  | Imsr of arith_operand
  (* eXtended immediate *)
  | Ximm of ximm

  | Indirect of register * A.offset
  (* another form of Indirect *)
  | Entity of A.entity

[@@deriving show]

(* ------------------------------------------------------------------------- *)
(* Instructions *)
(* ------------------------------------------------------------------------- *)

(* less: could probably factorize things and move stuff in Ast_asm.ml *)
type instr = 
  (* Arithmetic *)
  | Arith of arith_opcode * arith_option *
      arith_operand (* src *) * register option * register (* dst *)

  (* Memory *)
  | MOVE of A.move_size * move_option *
      mov_operand (* src *) * mov_operand (* dst *) (* virtual *)
  | SWAP of A.move_size (* actually only (Byte x) *) * 
       register (* indirect *) * register * register option

  (* Control flow *)
  | B  of A.branch_operand
  | BL of A.branch_operand
  | Cmp of cmp_opcode * arith_operand * register
  (* just Relative or LabelUse here for branch_operand *)
  | Bxx of condition * A.branch_operand (* virtual, sugar for B.XX *) 

  (* System *)
  | SWI of int (* value actually unused in Plan 9 and Linux *)
  | RFE (* virtual, sugar for MOVM *)


  and arith_opcode = 
    (* logic *)
    | AND | ORR | EOR
    (* arithmetic *)
    | ADD | SUB   | MUL   | DIV | MOD (* DIV and MOD are virtual *)
    (* bit shifting; immediate operand can only be between 0 and 31 *)
    | SLL | SRL | SRA (* virtual, sugar for bitshift register *)
    (* less useful *)
    | BIC  | ADC | SBC  | RSB | RSC
    (* middle operand always empty (could lift up and put special type) *)
    | MOV | MVN (* MOV has no reading syntax in 5a, MOVE is used *)
  and arith_option = arith_cond option
   and arith_cond = Set_condition (* .S *)

  and cmp_opcode = 
    | CMP
    (* less useful *)
    | TST | TEQ | CMN

  and condition =
    (* equal, not equal *)
    | EQ | NE
    (* greater than, less than, greater or equal, less or equal *)
    | GT of sign | LT of sign | GE of sign | LE of sign
    (* minus/negative, plus/positive *)
    | MI | PL 
    (* overflow set/clear *)
    | VS | VC
    (* always/never *)
    | AL | NV

   and move_option = move_cond option
     (* this is used only with a MOV with an indirect with offset operand *)
     and move_cond = WriteAddressBase (* .W *) | PostOffsetWrite (* .P *)
[@@deriving show]

(* ------------------------------------------------------------------------- *)
(* Program *)
(* ------------------------------------------------------------------------- *)

(* On the ARM every instructions can be prefixed with a condition.
 * Note that cond should be AL (Always) for B/Bxx instructions.
*)
type instr_with_cond = instr * condition
[@@deriving show]

type line = instr_with_cond A.line
[@@deriving show]

type program = instr_with_cond A.program
[@@deriving show]

(*****************************************************************************)
(* Extractors/Visitors *)
(*****************************************************************************)

let branch_opd_of_instr (instr : instr_with_cond) : A.branch_operand option =
  (* less: could issue warning if cond <> AL when B or Bxx, or normalize? *)
  match fst instr with
  (* ocaml-light: | B opd | BL opd | Bxx (_, opd) -> *)
  | B opd -> Some opd
  | BL opd -> Some opd
  | Bxx (_cond, opd) -> Some opd
  | Arith _ | MOVE _ | SWAP _ | Cmp _ | SWI _ | RFE -> None

let visit_globals_instr (f : global -> unit) (i : instr_with_cond) : unit =
  let mov_operand x =
    match x with
    | Entity (A.Global (x, _)) -> f x
    | Entity (A.Param _ | A.Local _) -> ()
    | Ximm x -> A.visit_globals_ximm f x
    | Imsr _ | Indirect _ -> ()
  in
  match fst i with
  | MOVE (_, _, m1, m2) -> mov_operand m1; mov_operand m2
  (* ocaml-light: | B b | BL b | Bxx (_, b) -> branch_operand b *)
  | B b -> A.visit_globals_branch_operand f b
  | BL b -> A.visit_globals_branch_operand f b
  | Bxx (_, b) -> A.visit_globals_branch_operand f b
  | Arith _ | SWAP _ | Cmp _ | SWI _ | RFE -> () 
