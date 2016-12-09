
type integer = int

type env = {
  ids:  (Ast.fullname, idinfo) Hashtbl.t;
  structs: (Ast.fullname, Type.struct_kind * Type.structdef) Hashtbl.t;
  typedefs: (Ast.fullname, Type.t) Hashtbl.t;
  enums: (Ast.fullname, Type.integer_type) Hashtbl.t;
  constants: (Ast.fullname, integer * Type.integer_type) Hashtbl.t;

  (* used only internally *)
  current_function_type: (Type.t * Type.t list * bool);
  expr_context: expr_context;
}
  and idinfo = {
    typ: Type.t;
    sto: Storage.t;
    loc: Location_cpp.loc;
    (* typed initialisers *)
    ini: Ast.initialiser option;
  }
 and expr_context = CtxWantValue | CtxGetRef | CtxSizeof

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
  Ast.program -> env * Ast.func_def list
