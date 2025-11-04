(*s: Arch_compiler.ml *)
(* Copyright 2016 Yoann Padioleau, see copyright.txt *)

(*s: type [[Arch_compiler.env]] *)
type env = {
  structs: (Ast.fullname, Type.struct_kind * Type.structdef) Hashtbl.t;
}
(*e: type [[Arch_compiler.env]] *)

(*s: type [[Arch_compiler.t]] *)
type t = {
  width_of_type: env -> Type.t -> int;
}
(*e: type [[Arch_compiler.t]] *)

(* todo? have a portable asm? ast_asm_common.ml? 
with ATEXT, ANOP, ARET, etc.
*)
(*e: Arch_compiler.ml *)
