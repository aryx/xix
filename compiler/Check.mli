
type error =
  | Inconsistent of 
      string * Location_cpp.loc * (* error here *) 
      string * Location_cpp.loc   (* previous decl/def/whatever here *)
  | Misc of string * Location_cpp.loc

val string_of_error: error -> string

exception Error of error
val failhard : bool ref

(* can raise Error if failhard, otherwise print on stderr *)
val check_program: Ast.program -> unit
