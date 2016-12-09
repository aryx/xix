(* Copyright 2016 Yoann Padioleau, see copyright.txt *)

type env = {
  structs: (Ast.fullname, Type.struct_kind * Type.structdef) Hashtbl.t;
}

type arch = {
  width_of_type: env -> Type.t -> int;
}

(* todo? have a portable asm? ast_asm_common.ml? 
with ATEXT, ANOP, ARET, etc.
*)
