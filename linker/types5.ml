open Common

open Ast_asm5
module A = Ast_asm5
module T = Types

(* Split Asm5 instructions in code vs data *)

(* For 'code' below we want to do some naming. We could copy many of ast_asm5.ml
 * and replace 'entity' with the fully resolved 'symbol'.
 * But it would be a big copy paste. Instead, we opted for a mutable field 
 * in ast_asm5.ml set by the linker (see Ast_asm5.entity.priv).
 *)

type code = (instr * Ast_asm5.pos)
and instr =
  | TEXT of A.entity * A.attributes * int
  | WORD of A.imm_or_ximm
  | I of A.instr * A.condition

(* remember that GLOBL information is stored in symbol table  *)
type data = 
  | DATA of A.entity * A.offset * int * A.imm_or_ximm


(* graph via pointers, like in original 5l *)
type node = {
  mutable node: instr;
  mutable next: node option;
  mutable branch: node option;
}

type code_graph = node (* the first node *)

(* assert not Some -1 ! should have been set during loading! *)
let symbol_of_entity e =
  e.name, (match e.priv with None -> T.Public | Some i -> T.Private i)

