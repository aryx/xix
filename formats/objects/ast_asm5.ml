(* Copyright 2015, 2016 Yoann Padioleau, see copyright.txt *)
open Common

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 * Abstract Syntax Tree for the assembly language supported by 5a
 * which we call Asm5.
 *
 * Note that in plan9 object files are mostly the serialized form of 
 * the assembly AST which is why this file is in this directory.
 * 
 * TODO: remain debugging and extensions chapters
 *  - special bits, mv closer to relevant instr? Arith has only .S,
 *    Mov has only .P, .W, etc.
 *)

(*****************************************************************************)
(* The AST related types *)
(*****************************************************************************)

(* ------------------------------------------------------------------------- *)
(* Numbers and Strings *)
(* ------------------------------------------------------------------------- *)

(* line# *)
type pos = int 

(* enough for ARM 32 bits? on 64 bits machine it is enough :) *)
type integer = int 
(* increments by unit of 1 *)
type virtual_code_address = int
(* can be 0, negative, positive *)
type offset = int

type label = string
type symbol = string
type entity = symbol * bool (* static *)

(* ------------------------------------------------------------------------- *)
(* Operands *)
(* ------------------------------------------------------------------------- *)

type register = R of int (* between 0 and 15 *)
let rSB = R 12
let rSP = R 13
let rLINK = R 14
let rPC = R 15

type arith_operand =
  | Imm of integer (* characters are converted in integers *)
  | Reg of register
  | Shift of register * shift_reg_op * 
             (register, int (* between 0 and 31 *)) Common.either

  and shift_reg_op =
    | Sh_logic_left | Sh_logic_right
    | Sh_arith_right | Sh_rotate_right

type mov_operand = 
  (* Immediate shift register *)
  | Imsr of arith_operand
  (* eXtended immediate *)
  | Ximm of ximm

  | Indirect of register * offset
  (* those below are all specialized forms of Indirect *)
  | Param of symbol option * offset (* FP *)
  | Local of symbol option * offset (* SP *)
  (* stricter: we disallow anonymous offsets to SB *)
  | Entity of entity * offset (* SB *) 

  and ximm =
    | String of string (* limited to 8 characters *)
    (* Float? *)
    (* stricter: we disallow address of FP or SP, and offset to SB *)
    | Address of entity

type branch_operand =
  (* nireg *)
  | SymbolJump of entity * offset
  | IndirectJump of register

  (* rel *)
  (* before resolve *)
  | Relative of int (* relative to PC, in units of virtual_code_address *)
  | LabelUse of label * offset (* useful to have offset? *)
  (* after resolve *)
  | Absolute of virtual_code_address

(* less: could transform labels in symbols? but then need to keep
 * Relative jumps and remove Absolute.
 *)  

(* ------------------------------------------------------------------------- *)
(* Instructions *)
(* ------------------------------------------------------------------------- *)

type instr = 
  (* Arithmetic *)
  | Arith of arith_opcode * 
      arith_operand (* src *) * register option * register (* dst *)

  (* Memory *)
  | MOV of move_size * mov_operand * mov_operand (* virtual *)
  | SWAP of move_size (* actually only (Byte x) *) * 
       register (* indirect *) * register * register option

  (* Control flow *)
  | B of branch_operand
  | BL of branch_operand
  | RET (* virtual *)
  | Cmp of cmp_opcode * arith_operand * register
  (* TODO: normally just rel here, relative jump or label *)
  | Bxx of condition * branch_operand (* virtual, sugar *) 

  (* System *)
  | SWI of int (* value actually unused in plan9 *)
  | RFE (* virtual, sugar for MOVM *)

  (* Misc *)
  | NOP (* virtual, removed by linker, no reading syntax *)


  and arith_opcode = 
    (* logic *)
    | AND | ORR | EOR
    (* arith *)
    | ADD | SUB | MUL | DIV | MOD (* DIV and MOD are virtual *)
    | SLL | SRL | SRA (* virtual, sugar for bitshift register *)
    (* less useful *)
    | BIC  | ADC | SBC  | RSB | RSC
    (* middle operand always empty (could lift up and put special type) *)
    | MVN 
  and cmp_opcode = 
    | CMP
    (* less useful *)
    | TST | TEQ | CMN

  and condition =
    (* equal, not equal *)
    | EQ | NE
    (* greater, less, greater or, less or equal *)
    | GT of sign | LT of sign | GE of sign | LE of sign
    (* minus/negative plus/positive *)
    | MI | PL 
    (* overflow set/clear *)
    | VS | VC
    (* always/never *)
    | AL | NV

   (* sign is relevant only for a load operation, not a store *)
   and sign = Signed | Unsigned
   and move_size = Byte of sign | Word | HalfWord of sign

type pseudo_instr =
  (* stricter: we allow only SB for TEXT and GLOBL, and no offset *)
  | TEXT of entity * attributes * int (* size locals *)
  | GLOBL of entity (* can have offset? *) * attributes * int (* size *)
  | DATA of entity * offset * int (* size *) * imm_or_ximm
  (* any ximm? even String? And Float? for float should have DWORD? *)
  | WORD of imm_or_ximm

  and attributes = attribute list
  and attribute = DUPOK | NOPROF

  and imm_or_ximm = (integer, ximm) Common.either

(* ------------------------------------------------------------------------- *)
(* Program *)
(* ------------------------------------------------------------------------- *)

type line = 
  | Pseudo of pseudo_instr
  | Instr of instr * condition (* TODO bitset list *)
  (* disappear after resolve *)
  | LabelDef of label
  (* ex: #line 20 "foo.c" *)
  | LineDirective of int * Common.filename

type program = (line * pos) list
