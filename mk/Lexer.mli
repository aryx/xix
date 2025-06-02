(*s: Lexer.mli *)

(* lexer state *)
type state = 
  | Start

  (* once we started to parse a rule, the next newline will start a recipe *)
  | AfterColon
  (* the lexing rules are different in a recipe; we do not parse rc's input *)
  | InRecipe

  (* once we started to parse an assign, the second = is like a string *)
  | AfterEq
  (* except inside ${x:...=...} where we still want = to be TEq *)
  | InBrace

val state_ : state ref

val token: Lexing.lexbuf -> Parser.token

val recipe: Lexing.lexbuf -> Parser.token

(*e: Lexer.mli *)
