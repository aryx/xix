
(* An object file (.o) in Plan 9 is really just a serialized assembly AST *)
type 'instr t = {
  prog:  'instr Ast_asm.program;
  arch: Arch.t
}

exception WrongVersion
(* used also in Library_file.ml *)
val version : int 

(* may raise WrongVersion *)
val load: Chan.i (* obj file *) -> 'instr t

val save: Arch.t -> 'instr Ast_asm.program -> Chan.o (* obj file *) -> unit

(* look whether the filename finishes in .o[5vi] *)
val is_obj_filename : Fpath.t -> bool
