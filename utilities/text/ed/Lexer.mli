
val token: Lexing.lexbuf -> Token.t

val regexp: char -> Lexing.lexbuf -> string

val filename: Lexing.lexbuf -> string
val line: Lexing.lexbuf -> string

(* ugly: should not be needed outside *)
val buf: Buffer.t
