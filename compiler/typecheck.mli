
type integer = int

type env = {
  ids:  (Ast.fullname, Type.t * Storage.t * Location_cpp.loc) Hashtbl.t;
  tags: (Ast.fullname, Type.tagdef) Hashtbl.t;
  typedefs: (Ast.fullname, Type.t) Hashtbl.t;
  constants: (Ast.fullname, integer) Hashtbl.t;
}

type error = Check.error

val string_of_error: error -> string

exception Error of error


(* can raise Error
 * todo: return also modified Ast.program?
 *)
val check_program: Ast.program -> env

