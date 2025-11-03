(*s: libraries/Library_file.mli *)
(* An archive (.a) is really essentially just a list of objects, which in Plan 9
 * are just a list of serialized assembly ASTs
 *)
(*s: type [[Library_file.t (libraries/Library_file.mli)]] *)
type 'instr t = 'instr Object_file.t list
(*e: type [[Library_file.t (libraries/Library_file.mli)]] *)

(*s: signature [[Library_file.load]] *)
(* may raise Object_file.WrongVersion *)
val load:  Chan.i -> 'instr t
(*e: signature [[Library_file.load]] *)

(*s: signature [[Library_file.save]] *)
val save : 'instr t -> Chan.o -> unit
(*e: signature [[Library_file.save]] *)

(*s: signature [[Library_file.is_lib_filename]] *)
(* look whether finishes in .oa[5vi] or .oa *)
val is_lib_filename: Fpath.t -> bool
(*e: signature [[Library_file.is_lib_filename]] *)
(*e: libraries/Library_file.mli *)
