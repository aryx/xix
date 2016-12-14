
type error = Check.error
val string_of_error: error -> string
exception Error of error

(* can raise Error *)
val codegen: 
  (Ast.fullname, Typecheck.idinfo) Hashtbl.t *
  (Ast.fullname, Type.struct_kind * Type.structdef) Hashtbl.t * 
  Ast.func_def list 
  -> 
  Ast_asm5.program
