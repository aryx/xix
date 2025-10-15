
type token_category =
  | Eof
  | Sharp
  | Ident of string
  | Other

type ('token, 'ast) hook = {
  lexer: Lexing.lexbuf -> 'token;
  parser: (Lexing.lexbuf -> 'token) -> Lexing.lexbuf -> 'ast;
  category: 'token -> token_category;
  eof: 'token;
}

(* [parse] will [open_in] the passed file parameter but may also [open_in]
 * other files as [parse] will recursively process [#include] directives.
 *)
val parse: 
  < Cap.open_in; .. > ->
  ('token, 'ast) hook -> Preprocessor.conf -> Fpath.t -> 'ast

(* internals:
type macro = {
  name: string;
  nbargs: int option;
  varargs: bool; (* use "..." *)
  body: string;
}

val hmacros: (string, macro) Hashtbl.t

val define_cmdline_def: (string * string) -> unit

(* may raise an exception if macro was already defined *)
val define: Ast_cpp.macro -> unit

(* may raise an exception if the file could not be found *)
val find_include:
  include_paths -> (string * bool (* system header (<>) *)) -> Common.filename
*)
