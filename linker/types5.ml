(* Copyright 2016 Yoann Padioleau, see copyright.txt *)
open Common

module A = Ast_asm5
module T = Types

(* a single line number is not enough anymore, we need also the filename *)
type loc = Common.filename * Ast_asm5.loc
 (* with tarzan *)

(* Split Asm5 instructions in code vs data.
 *
 * For 'code' below we want to do some naming. We could copy many of 
 * ast_asm5.ml and replace 'global' with the fully resolved 'symbol'.
 * But it would be a big copy paste. Instead, we opted for a mutable field 
 * in ast_asm5.ml set by the linker (see Ast_asm5.entity.priv).
 *)
type code = (instr * loc)
(* a subset of Ast_asm5.line (no GLOBL/DATA, no LabelDef/LineDirective) *)
and instr =
  | TEXT of A.global * A.attributes * int
  | WORD of A.imm_or_ximm
  | I of A.instr * A.condition
 (* with tarzan *)

(* remember that GLOBL information is stored in symbol table  *)
type data = 
  | DATA of A.global * A.offset * int * A.imm_or_ximm


(* graph via pointers, like in original 5l *)
type node = {
  (* can be altered during rewriting *)
  mutable instr: instr;
  mutable next: node option;
  (* for branching instructions and also for instructions using the pool *)
  mutable branch: node option;
  
  (* set after layout_text *)
  mutable real_pc: T.real_pc;

  loc: loc;
}

type code_graph = node (* the first node *)



(* assert not Some -1 ! should have been set during loading! *)
let symbol_of_global e =
  e.A.name, (match e.A.priv with None -> T.Public | Some i -> T.Private i)

let lookup_global x h =
  let symbol = symbol_of_global x in
  T.lookup symbol x.A.signature h


(* less: would need Hist mapping for this file to convert to original source *)
let s_of_loc (file, line) =
  spf "%s:%d" file line

let s_of_global x = 
  x.A.name ^ (match x.A.priv with None -> "" | Some _ -> "<>")


let rec iter f n =
  f n;
  n.next |> Common.if_some (fun n -> iter f n)

let rec iter_with_env f env n =
  let env = f env n in
  n.next |> Common.if_some (fun n -> iter_with_env f env n)
