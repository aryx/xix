(*s: Check.mli *)

(*s: type [[Check.error (Check.mli)]] *)
type error =
  | Inconsistent of 
      string * Location_cpp.loc * (* error here *) 
      string * Location_cpp.loc   (* previous decl/def/whatever here *)
  | Misc of string * Location_cpp.loc
(*e: type [[Check.error (Check.mli)]] *)

(*s: signature [[Check.string_of_error]] *)
val string_of_error: error -> string
(*e: signature [[Check.string_of_error]] *)

(*s: exception [[Check.Error (Check.mli)]] *)
exception Error of error
(*e: exception [[Check.Error (Check.mli)]] *)
(*s: signature [[Check.failhard]] *)
val failhard : bool ref
(*e: signature [[Check.failhard]] *)

(*s: signature [[Check.check_program]] *)
(* can raise Error if failhard, otherwise print on stderr *)
val check_program: Ast.program -> unit
(*e: signature [[Check.check_program]] *)
(*e: Check.mli *)
