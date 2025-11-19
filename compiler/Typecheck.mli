(*s: Typecheck.mli *)

(*s: type [[Typecheck.idinfo]] *)
type idinfo = {
    typ: Type.t;
    sto: Storage.t;
    loc: Location_cpp.loc;
    (* typed initialisers (fake expression for function definitions) *)
    ini: Ast.initialiser option;
  }
(*e: type [[Typecheck.idinfo]] *)

(*s: type [[Typecheck.typed_program]] *)
type typed_program = {
  (* resolved type and storage information for identifiers and tags *)
  ids: (Ast.fullname, idinfo) Hashtbl.t;

  (* resolved struct definitions *)
  structs: (Ast.fullname, Type.struct_kind * Type.structdef) Hashtbl.t;

  (* functions annotated with types for each expression nodes
   * (so you can more easily generate code later).
   * 
   * The enum constants should also be internally resolved and replaced
   * with constants and some constant expressions (e.g., for
   * array size) should also be resolved (and evaluated).
   *)
  funcs: Ast.func_def list;
}
(*e: type [[Typecheck.typed_program]] *)

(*s: type [[Typecheck.error]] *)
type error = Check.error
(*e: type [[Typecheck.error]] *)
(*s: signature [[Typecheck.string_of_error]] *)
val string_of_error: error -> string
(*e: signature [[Typecheck.string_of_error]] *)
(*s: exception [[Typecheck.Error]] *)
exception Error of error
(*e: exception [[Typecheck.Error]] *)

(*s: signature [[Typecheck.check_and_annotate_program]] *)
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
  Ast.program -> typed_program
(*e: signature [[Typecheck.check_and_annotate_program]] *)
(*e: Typecheck.mli *)
