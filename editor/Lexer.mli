(*s: Lexer.mli *)
(*s: signature [[Lexer.token]] *)
val token: Lexing.lexbuf -> Token.t
(*e: signature [[Lexer.token]] *)
(*s: signature [[Lexer.regexp]] *)
val regexp: char -> Lexing.lexbuf -> string
(*e: signature [[Lexer.regexp]] *)
(*s: signature [[Lexer.filename]] *)
val filename: Lexing.lexbuf -> string
(*e: signature [[Lexer.filename]] *)
(*s: signature [[Lexer.line]] *)
val line: Lexing.lexbuf -> string
(*e: signature [[Lexer.line]] *)

(*s: signature [[Lexer.buf]] *)
(* ugly: should not be needed outside *)
val buf: Buffer.t
(*e: signature [[Lexer.buf]] *)
(*e: Lexer.mli *)
