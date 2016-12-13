
val codegen: 
  (Ast.fullname, Typecheck.idinfo) Hashtbl.t *
  (Ast.fullname, Type.struct_kind * Type.structdef) Hashtbl.t * 
  Ast.func_def list 
  -> 
  Ast_asm5.program
