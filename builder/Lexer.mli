(*s: Lexer.mli *)

(*s: type [[Lexer.state]] *)
(* lexer state *)
type state = 
  | Start
  (* once we started to parse a rule, the next newline will start a recipe *)
  | AfterColon
  (* the lexing rules are different in a recipe; we do not parse rc's input *)
  | InRecipe
  (*s: [[Lexer.state]] other cases *)
  (* once we started to parse an assign, the second = is like a string *)
  | AfterEq
  (*x: [[Lexer.state]] other cases *)
  (* except inside ${x:...=...} where we still want = to be TEq *)
  | InBrace
  (*e: [[Lexer.state]] other cases *)
(*e: type [[Lexer.state]] *)
(*s: signature [[Lexer.state_]] *)
val state_ : state ref
(*e: signature [[Lexer.state_]] *)

(*s: signature [[Lexer.token]] *)
val token: Lexing.lexbuf -> Parser.token
(*e: signature [[Lexer.token]] *)
(*s: signature [[Lexer.recipe]] *)
val recipe: Lexing.lexbuf -> Parser.token
(*e: signature [[Lexer.recipe]] *)
(*e: Lexer.mli *)
