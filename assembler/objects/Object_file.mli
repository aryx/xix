
(* An object file (.o) in Plan 9 is really just a serialized assembly AST *)
type 'instr t = 'instr Ast_asm.program

exception WrongVersion
(* used also in Library_file.ml *)
val version : int 

(* may raise WrongVersion *)
val load: Chan.i (* obj file *) -> 'instr t

val save: 'instr t -> Chan.o (* obj file *) -> unit

(* look whether the filename finishes in .o[5vi] *)
val is_obj_filename : Fpath.t -> bool
