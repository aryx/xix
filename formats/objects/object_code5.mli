
type object_code = 
    Ast_asm5.program * 
    Location_cpp.location_history list (* includes src file origin *)

val save: 
  object_code -> Common.filename (* obj file *) -> unit

exception WrongVersion

(* may raise WrongVersion *)
val load: 
  Common.filename (* obj file *) -> object_code
