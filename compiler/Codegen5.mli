(*s: Codegen5.mli *)

(*s: type [[Codegen5.error (Codegen5.mli)]] *)
type error = Check.error
(*e: type [[Codegen5.error (Codegen5.mli)]] *)
(*s: signature [[Codegen5.string_of_error]] *)
val string_of_error: error -> string
(*e: signature [[Codegen5.string_of_error]] *)
(*s: exception [[Codegen5.Error (Codegen5.mli)]] *)
exception Error of error
(*e: exception [[Codegen5.Error (Codegen5.mli)]] *)

(*s: signature [[Codegen5.codegen]] *)
(* can raise Error *)
val codegen: 
  (Ast.fullname, Typecheck.idinfo) Hashtbl.t *
  (Ast.fullname, Type.struct_kind * Type.structdef) Hashtbl.t * 
  Ast.func_def list 
  -> 
  Ast_asm5.program
(*e: signature [[Codegen5.codegen]] *)
(*e: Codegen5.mli *)
