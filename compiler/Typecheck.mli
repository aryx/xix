
type idinfo = {
    typ: Type_.t;
    sto: Storage.t;
    loc: Location_cpp.loc;
    (* typed initialisers *)
    ini: Ast.initialiser option;
  }

type error = Check.error
val string_of_error: error -> string
exception Error of error

(* Returns resolved type and storage information for identifiers and tags.
 * Annotate also with types each expression nodes in the returned functions
 * (so you can more easily generate code later).
 * 
 * It also internally resolves enum constants and replaces them
 * with constants and evaluates some constant expressions (e.g., for
 * array size).
 * 
 * can raise Error.
 *)
val check_and_annotate_program: 
  Ast.program -> 
  (Ast.fullname, idinfo) Hashtbl.t *
  (Ast.fullname, Type_.struct_kind * Type_.structdef) Hashtbl.t *
  Ast.func_def list
