(*s: objects/Ast_asm5.ml *)
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
 * !!! If you modify this file please increment Object_file.version !!!
 * 
 * TODO: 
 *  - 5c-only opcodes? CASE, BCASE, MULU/DIVU/MODU (or better in Ast_asm.ml too?)
 *  - MULA, MULL,
 *  - MOVM (and his special bits .IA/...), 
 *  - PSR, MCR/MRC,
 *  - handle the instructions used in the kernel
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
(*s: type [[Ast_asm5.reg]] *)
type reg = A.register (* between 0 and 15 *)
(*e: type [[Ast_asm5.reg]] *)
[@@deriving show]

(*s: type [[Ast_asm5.freg]] *)
type freg = A.fregister (* between 0 and 15 *)
(*e: type [[Ast_asm5.freg]] *)
[@@deriving show]

(* ?? *)
(*s: type [[Ast_asm5.creg]] *)
type creg = C of int (* between 0 and 15 *)
(*e: type [[Ast_asm5.creg]] *)

(* reserved by linker *)
(*s: constant [[Ast_asm5.rTMP]] *)
let rTMP = R 11
(*e: constant [[Ast_asm5.rTMP]] *)
(*s: constant [[Ast_asm5.rSB]] *)
let rSB  = R 12
(*e: constant [[Ast_asm5.rSB]] *)
(*s: constant [[Ast_asm5.rSP]] *)
let rSP  = R 13
(*e: constant [[Ast_asm5.rSP]] *)
(* reserved by hardware *)
(*s: constant [[Ast_asm5.rLINK]] *)
let rLINK = R 14
(*e: constant [[Ast_asm5.rLINK]] *)
(*s: constant [[Ast_asm5.rPC]] *)
let rPC   = R 15
(*e: constant [[Ast_asm5.rPC]] *)

(*s: constant [[Ast_asm5.nb_registers]] *)
let nb_registers = 16
(*e: constant [[Ast_asm5.nb_registers]] *)

(*s: type [[Ast_asm5.arith_operand]] *)
type arith_operand =
  | Imm of A.integer (* characters are converted to integers *)
  | Reg of reg
  (* can not be used with shift opcodes (SLL/SRL/SRA) *)
  | Shift of reg * shift_reg_op * 
             (reg, int (* between 0 and 31 *)) Either_.t
(*e: type [[Ast_asm5.arith_operand]] *)

(*s: type [[Ast_asm5.shift_reg_op]] *)
  and shift_reg_op =
    | Sh_logic_left | Sh_logic_right
    | Sh_arith_right | Sh_rotate_right
(*e: type [[Ast_asm5.shift_reg_op]] *)
[@@deriving show]

(* alt: could almost be moved to Ast_asm.ml but Shift above of arith_operand
 * seems arm-specific
 *)
(*s: type [[Ast_asm5.mov_operand]] *)
type mov_operand = 
  (* Immediate shift register *)
  | Imsr of arith_operand
  (* eXtended immediate.
   * (Ximm (Int x) is converted in Imsr (Imm x) in the parser
   *)
  | Ximm of ximm

  | Indirect of reg * A.offset
  (* another form of Indirect *)
  | Entity of A.entity
(*e: type [[Ast_asm5.mov_operand]] *)

[@@deriving show]

(* ------------------------------------------------------------------------- *)
(* Instructions *)
(* ------------------------------------------------------------------------- *)

(* less: could probably factorize things and move stuff in Ast_asm.ml *)
(*s: type [[Ast_asm5.instr]] *)
type instr = 
  (* Arithmetic *)
  (*s: [[Ast_asm5.instr]] arithmetic instructions cases *)
  | Arith of arith_opcode * arith_cond option *
      arith_operand (* src *) * reg option * reg (* dst *)
  (*x: [[Ast_asm5.instr]] arithmetic instructions cases *)
  | ArithF of (arithf_opcode * A.floatp_precision) *
      (A.floatp, freg) Either_.t * freg option * freg
  (*e: [[Ast_asm5.instr]] arithmetic instructions cases *)

  (* Memory *)
  (*s: [[Ast_asm5.instr]] memory instructions cases *)
  | MOVE of A.move_size * move_option *
      mov_operand (* src *) * mov_operand (* dst *) (* virtual *)
  | SWAP of A.move_size (* actually only (Byte x) *) * 
       reg (* indirect *) * reg * reg option
  (*e: [[Ast_asm5.instr]] memory instructions cases *)

  (* Control flow *)
  (*s: [[Ast_asm5.instr]] control-flow instructions cases *)
  | B  of A.branch_operand (* branch *)
  | BL of A.branch_operand (* branch and link *)
  | Cmp of cmp_opcode * arith_operand * reg
  (* just Relative or LabelUse here for branch_operand *)
  | Bxx of condition * A.branch_operand (* virtual, sugar for B.XX *) 
  (*x: [[Ast_asm5.instr]] control-flow instructions cases *)
  | CmpF of A.floatp_precision * freg * freg
  (*e: [[Ast_asm5.instr]] control-flow instructions cases *)

  (* System *)
  (*s: [[Ast_asm5.instr]] system instructions cases *)
  | SWI of int (* value actually unused in Plan 9 and Linux *)
  | RFE (* virtual, sugar for MOVM *)
  (*e: [[Ast_asm5.instr]] system instructions cases *)
(*e: type [[Ast_asm5.instr]] *)

(*s: type [[Ast_asm5.arith_opcode]] *)
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
(*e: type [[Ast_asm5.arith_opcode]] *)
(*s: type [[Ast_asm5.arith_cond]] *)
  and arith_cond = Set_condition (* .S *)
(*e: type [[Ast_asm5.arith_cond]] *)

(*s: type [[Ast_asm5.arithf_opcode]] *)
  and arithf_opcode =
    | ADD_ | SUB_ | MUL_ | DIV_
(*e: type [[Ast_asm5.arithf_opcode]] *)
    
(*s: type [[Ast_asm5.cmp_opcode]] *)
  and cmp_opcode = 
    | CMP
    (* less useful *)
    | TST | TEQ | CMN
(*e: type [[Ast_asm5.cmp_opcode]] *)

(*s: type [[Ast_asm5.condition]] *)
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
(*e: type [[Ast_asm5.condition]] *)

(*s: type [[Ast_asm5.move_option]] *)
   and move_option = move_cond option
(*e: type [[Ast_asm5.move_option]] *)
     (* this is used only with a MOV with an indirect with offset operand *)
(*s: type [[Ast_asm5.move_cond]] *)
     and move_cond = WriteAddressBase (* .W *) | PostOffsetWrite (* .P *)
(*e: type [[Ast_asm5.move_cond]] *)

[@@deriving show]

(* ------------------------------------------------------------------------- *)
(* Program *)
(* ------------------------------------------------------------------------- *)

(*s: type [[Ast_asm5.instr_with_cond]] *)
type instr_with_cond = instr * condition
(*e: type [[Ast_asm5.instr_with_cond]] *)
[@@deriving show]

(*s: type [[Ast_asm5.program]] *)
(* On the ARM every instructions can be prefixed with a condition.
 * Note that cond should be AL (Always) for B/Bxx instructions.
*)
type program = instr_with_cond A.program
(*e: type [[Ast_asm5.program]] *)
[@@deriving show]

(*****************************************************************************)
(* Extractors/Visitors *)
(*****************************************************************************)

(*s: function [[Ast_asm5.branch_opd_of_instr]] *)
let branch_opd_of_instr (instr : instr_with_cond) : A.branch_operand option =
  (* less: could issue warning if cond <> AL when B or Bxx, or normalize? *)
  match fst instr with
  (* ocaml-light: | B opd | BL opd | Bxx (_, opd) -> *)
  | B opd -> Some opd
  | BL opd -> Some opd
  | Bxx (_cond, opd) -> Some opd
  | Arith _ | ArithF _ | MOVE _ | SWAP _ | Cmp _ | CmpF _ | SWI _ | RFE -> None
(*e: function [[Ast_asm5.branch_opd_of_instr]] *)

(*s: function [[Ast_asm5.visit_globals_instr]] *)
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
  | Arith _ | ArithF _ | SWAP _ | Cmp _ | CmpF _ | SWI _ | RFE -> () 
(*e: function [[Ast_asm5.visit_globals_instr]] *)
(*e: objects/Ast_asm5.ml *)
