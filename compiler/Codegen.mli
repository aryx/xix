(*s: Codegen.mli *)

(*s: type [[Codegen.error]] *)
type error = Check.error
(*e: type [[Codegen.error]] *)
(*s: signature [[Codegen.string_of_error]] *)
val string_of_error: error -> string
(*e: signature [[Codegen.string_of_error]] *)
(*s: exception [[Codegen.Error]] *)
exception Error of error
(*e: exception [[Codegen.Error]] *)

(*s: signature [[Codegen.codegen]] *)
(* can raise Error *)
val codegen: 
  Typecheck.typed_program ->  Ast_asm5.program
(*e: signature [[Codegen.codegen]] *)
(*e: Codegen.mli *)
