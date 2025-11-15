(*s: Codegen5.mli *)

(*s: type [[Codegen5.error]] *)
type error = Check.error
(*e: type [[Codegen5.error]] *)
(*s: signature [[Codegen5.string_of_error]] *)
val string_of_error: error -> string
(*e: signature [[Codegen5.string_of_error]] *)
(*s: exception [[Codegen5.Error]] *)
exception Error of error
(*e: exception [[Codegen5.Error]] *)

(*s: signature [[Codegen5.codegen]] *)
(* can raise Error *)
val codegen: 
  Typecheck.typed_program ->  Ast_asm5.program
(*e: signature [[Codegen5.codegen]] *)
(*e: Codegen5.mli *)
