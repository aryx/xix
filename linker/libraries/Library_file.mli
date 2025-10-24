(* An archive (.a) is really essentially just a list of objects, which in Plan 9
 * are just a list of serialized assembly ASTs
 *)
type 'instr t = 'instr Ast_asm.program list

val save : 'instr t -> Chan.o -> unit

(* may raise Object_file.WrongVersion *)
val load:  Chan.i -> 'instr t

(* look whether finishes in .oa[5vi] or .oa *)
val is_libfile: Fpath.t -> bool
