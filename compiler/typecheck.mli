
type integer = int

type env = {
  ids:  (Ast.fullname, Type.t * Storage.t * Location_cpp.loc) Hashtbl.t;
  structs: (Ast.fullname, Type.struct_kind * Type.structdef) Hashtbl.t;
  typedefs: (Ast.fullname, Type.t) Hashtbl.t;
  constants: (Ast.fullname, integer) Hashtbl.t;
}

type error = Check.error

val string_of_error: error -> string

exception Error of error


(* Returns resolved type and storage information for identifiers, tags,
 * resolved enum constants, constant expression evaluations.
 * Returns also Ast.program elements with annotated types at each node.
 * Basically returns everything you need to easily generate code.
 * can raise Error.
 *)
val check_and_annotate_program: 
  Ast.program -> env * Ast.func_def list * Ast.var_decl list
