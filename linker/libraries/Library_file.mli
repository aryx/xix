(* An archive (.a) is really essentially just a list of objects, which in Plan 9
 * are just a list of serialized assembly ASTs
 *)
type 'instr t = 'instr Object_file.t list

(* may raise Object_file.WrongVersion *)
val load:  Chan.i -> 'instr t

val save : 'instr t -> Chan.o -> unit

(* look whether finishes in .oa[5vi] or .oa *)
val is_lib_filename: Fpath.t -> bool
