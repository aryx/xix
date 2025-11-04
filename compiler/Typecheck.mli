(*s: Typecheck.mli *)

(*s: type [[Typecheck.idinfo (Typecheck.mli)]] *)
type idinfo = {
    typ: Type.t;
    sto: Storage.t;
    loc: Location_cpp.loc;
    (* typed initialisers *)
    ini: Ast.initialiser option;
  }
(*e: type [[Typecheck.idinfo (Typecheck.mli)]] *)

(*s: type [[Typecheck.error (Typecheck.mli)]] *)
type error = Check.error
(*e: type [[Typecheck.error (Typecheck.mli)]] *)
(*s: signature [[Typecheck.string_of_error]] *)
val string_of_error: error -> string
(*e: signature [[Typecheck.string_of_error]] *)
(*s: exception [[Typecheck.Error (Typecheck.mli)]] *)
exception Error of error
(*e: exception [[Typecheck.Error (Typecheck.mli)]] *)

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
  Ast.program -> 
  (Ast.fullname, idinfo) Hashtbl.t *
  (Ast.fullname, Type.struct_kind * Type.structdef) Hashtbl.t *
  Ast.func_def list
(*e: signature [[Typecheck.check_and_annotate_program]] *)
(*e: Typecheck.mli *)
