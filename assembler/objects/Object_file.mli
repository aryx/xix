(*s: objects/Object_file.mli *)

(*s: type [[Object_file.t]] *)
(* An object file (.o) in Plan 9 is really just a serialized assembly AST *)
type 'instr t = {
  prog:  'instr Ast_asm.program;
  arch: Arch.t
}
(*e: type [[Object_file.t]] *)

(*s: exception [[Object_file.WrongVersion]] *)
exception WrongVersion
(*e: exception [[Object_file.WrongVersion]] *)
(*s: signature [[Object_file.version]] *)
(* used also in Library_file.ml *)
val version : int 
(*e: signature [[Object_file.version]] *)

(*s: signature [[Object_file.load]] *)
(* may raise WrongVersion *)
val load: Chan.i (* obj file *) -> 'instr t
(*e: signature [[Object_file.load]] *)

(*s: signature [[Object_file.save]] *)
val save: Arch.t -> 'instr Ast_asm.program -> Chan.o (* obj file *) -> unit
(*e: signature [[Object_file.save]] *)

(*s: signature [[Object_file.is_obj_filename]] *)
(* look whether the filename finishes in .o[5vi] *)
val is_obj_filename : Fpath.t -> bool
(*e: signature [[Object_file.is_obj_filename]] *)
(*e: objects/Object_file.mli *)
