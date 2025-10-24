
val save: 
  'instr Ast_asm.program -> Chan.o (* obj file *) -> unit

exception WrongVersion

(* may raise WrongVersion *)
val load: 
  Chan.i (* obj file *) -> 'instr Ast_asm.program
