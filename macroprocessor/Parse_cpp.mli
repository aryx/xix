(*s: ../macroprocessor/Parse_cpp.mli *)

(*s: type [[Parse_cpp.token_category (../macroprocessor/Parse_cpp.mli)]] *)
type token_category =
  | Eof
  | Sharp
  | Ident of string
  | Other
(*e: type [[Parse_cpp.token_category (../macroprocessor/Parse_cpp.mli)]] *)
[@@deriving show]

(* wrapper around a parser/lexer (e.g., 5c) to preprocess first the file *)
(*s: type [[Parse_cpp.hook (../macroprocessor/Parse_cpp.mli)]] *)
type ('token, 'ast) hook = {
  lexer: Lexing.lexbuf -> 'token;
  parser: (Lexing.lexbuf -> 'token) -> Lexing.lexbuf -> 'ast;
  category: 'token -> token_category;
  eof: 'token;
}
(*e: type [[Parse_cpp.hook (../macroprocessor/Parse_cpp.mli)]] *)

(*s: signature [[Parse_cpp.parse]] *)
(* Wrapper function around a parser/lexer.
 *
 * Regarding Cap.open_in, [parse] will [open_in] the passed file parameter but
 * may also [open_in] other files as [parse] will recursively process
 * [#include] directives.
 *)
val parse: 
  < Cap.open_in; .. > ->
  ('token, 'ast) hook -> Preprocessor.conf -> Fpath.t -> 
  'ast * Location_cpp.location_history list
(*e: signature [[Parse_cpp.parse]] *)

(*e: ../macroprocessor/Parse_cpp.mli *)
