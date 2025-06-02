(*s: Lexer.mli *)
val token: Lexing.lexbuf -> Parser.token

exception Lexical_error of string
(*e: Lexer.mli *)
