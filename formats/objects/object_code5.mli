
type object_code = 
    Ast_asm5.program * Common.filename (* src file *)

val save: object_code -> Common.filename (* obj file *) -> unit

exception WrongVersion
(* may raise WrongVersion *)
val load: Common.filename (* obj file *) -> object_code
