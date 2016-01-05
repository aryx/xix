
type object_code = 
    Ast_asm5.program * Common.filename

val save: object_code -> Common.filename -> unit

exception WrongVersion
(* may raise WrongVersion *)
val load: Common.filename -> object_code
