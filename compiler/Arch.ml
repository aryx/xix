(* Copyright 2016 Yoann Padioleau, see copyright.txt *)

type env = {
  structs: (Ast.fullname, Type_.struct_kind * Type_.structdef) Hashtbl.t;
}

type t = {
  width_of_type: env -> Type_.t -> int;
}

(* todo? have a portable asm? ast_asm_common.ml? 
with ATEXT, ANOP, ARET, etc.
*)
