open Common

module A = Ast_asm5
module T = Types

(* a single line number is not enough anymore, we need also the filename *)
type loc = Common.filename * Ast_asm5.pos

(* Split Asm5 instructions in code vs data.
 *
 * For 'code' below we want to do some naming. We could copy many of 
 * ast_asm5.ml and replace 'entity' with the fully resolved 'symbol'.
 * But it would be a big copy paste. Instead, we opted for a mutable field 
 * in ast_asm5.ml set by the linker (see Ast_asm5.entity.priv).
 *)
type code = (instr * loc)
and instr =
  | TEXT of A.entity * A.attributes * int
  | WORD of A.imm_or_ximm
  | I of A.instr * A.condition

(* remember that GLOBL information is stored in symbol table  *)
type data = 
  | DATA of A.entity * A.offset * int * A.imm_or_ximm


(* graph via pointers, like in original 5l *)
type node = {
  (* can be altered during rewriting *)
  mutable node: instr;
  mutable next: node option;
  mutable branch: node option;
  
  (* set after layout_text *)
  mutable real_pc: T.real_pc;

  loc: loc;
}

type code_graph = node (* the first node *)





(* assert not Some -1 ! should have been set during loading! *)
let symbol_of_entity e =
  e.A.name, (match e.A.priv with None -> T.Public | Some i -> T.Private i)

let lookup_ent ent h =
  let symbol = symbol_of_entity ent in
  T.lookup symbol ent.A.signature h

(* less: would need Hist mapping for this file to convert to original source *)
let s_of_loc (file, line) =
  spf "%s:%d" file line

let s_of_ent ent = 
  ent.A.name ^ (match ent.A.priv with None -> "" | Some _ -> "<>")

let rec iter f n =
  f n;
  n.next |> Common.if_some (iter f)

let rec iter_with_env f env n =
  let env = f env n in
  n.next |> Common.if_some (fun n -> iter_with_env f env n)
