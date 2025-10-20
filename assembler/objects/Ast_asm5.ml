(* Copyright 2015, 2016 Yoann Padioleau, see copyright.txt *)
open Common

open Ast_asm

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Abstract Syntax Tree (AST) for the assembly language supported by 5a.
 * I call this language Asm5.
 *
 * Note that many types are now defined in Ast_asm.ml instead because they are
 * mostly arch independent and can be reused in other plan9 assemblers
 * (e.g,, 8a, 6a, va).
 *
 * Note that in Plan 9 object files are mostly the serialized form of 
 * the assembly AST, which is why this file is in this directory.
 * 
 * !!! If you modify this file please increment Object5.version !!!
 * 
 * TODO: 
 *  - floats (or better in Ast_asm.ml?)
 *  - MULA, MULL,
 *  - MOVM (and his special bits .IA/...), 
 *  - PSR, MCR/MRC,
 *  - 5c-only opcodes? CASE, BCASE, MULU/DIVU/MODU (or better in Ast_asm.ml too?)
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

type register = Ast_asm.register (* between 0 and 15 on ARM *)
[@@deriving show]

(* reserved by linker *)
let rTMP = R 11
let rSB  = R 12
let rSP  = R 13
(* reserved by hardware *)
let rLINK = R 14
let rPC   = R 15

let nb_registers = 16

type arith_operand =
  | Imm of integer (* characters are converted to integers *)
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

  | Indirect of register * offset
  (* another form of Indirect *)
  | Entity of entity

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
  | MOVE of move_size * move_option *
      mov_operand (* src *) * mov_operand (* dst *) (* virtual *)
  | SWAP of move_size (* actually only (Byte x) *) * 
       register (* indirect *) * register * register option

  (* Control flow *)
  | B  of branch_operand
  | BL of branch_operand
  | RET (* virtual *)
  | Cmp of cmp_opcode * arith_operand * register
  (* just Relative or LabelUse here for branch_operand *)
  | Bxx of condition * branch_operand (* virtual, sugar for B.XX *) 

  (* System *)
  | SWI of int (* value actually unused in Plan 9 and Linux *)
  | RFE (* virtual, sugar for MOVM *)

  (* Misc *)
  | NOP (* virtual, removed by linker, no reading syntax *)


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

   (* In a MOVE, sign is relevant only for a load operation *)
   and sign = Signed | Unsigned

   and move_size = Word | HalfWord of sign | Byte of sign
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

type line = instr_with_cond Ast_asm.line
[@@deriving show]

type program = instr_with_cond Ast_asm.program
[@@deriving show]

(*****************************************************************************)
(* Extractors/Visitors *)
(*****************************************************************************)

let branch_opd_of_instr (instr : instr_with_cond) : branch_operand option =
  (* less: could issue warning if cond <> AL when B or Bxx, or normalize? *)
  match (fst instr) with
  (* ocaml-light: | B opd | BL opd | Bxx (_, opd) -> *)
  | B opd -> Some opd
  | BL opd -> Some opd
  | Bxx (_cond, opd) -> Some opd
  | Arith _ | MOVE _ | SWAP _ | RET | Cmp _ | SWI _ | RFE | NOP -> None
