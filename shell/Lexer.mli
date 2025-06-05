(*s: Lexer.mli *)
(*s: signature [[Lexer.token]] *)
val token: Lexing.lexbuf -> Parser.token
(*e: signature [[Lexer.token]] *)

(*s: exception [[Lexer.Lexical_error]] *)
exception Lexical_error of string
(*e: exception [[Lexer.Lexical_error]] *)
(*e: Lexer.mli *)
