
type t = 
  Ast_asm5.program * 
  Location_cpp.location_history list (* includes src file origin *)

val save5: 
  t -> Chan.o (* obj file *) -> unit

exception WrongVersion

(* may raise WrongVersion *)
val load5: 
  Chan.i (* obj file *) -> t
