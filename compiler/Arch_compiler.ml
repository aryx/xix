(*s: Arch_compiler.ml *)
(* Copyright 2016 Yoann Padioleau, see copyright.txt *)

(*s: type [[Arch_compiler.env]] *)
type env = {
  (* same than Typecheck.typed_program.structs? *)
  structs: (Ast.fullname, Type.struct_kind * Type.structdef) Hashtbl.t;
}
(*e: type [[Arch_compiler.env]] *)

(*s: type [[Arch_compiler.t]] *)
type t = {
  width_of_type: env -> Type.t -> int;
}
(*e: type [[Arch_compiler.t]] *)

(* TODO? instead of Arch5, ... could also define an of_arch function
 * here, like I did for Arch_linker
 *)

(* todo? have a portable asm? ast_asm_common.ml? 
with ATEXT, ANOP, ARET, etc.
*)
(*e: Arch_compiler.ml *)
