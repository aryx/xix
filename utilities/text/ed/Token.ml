(*s: Token.ml *)
(* The tokens *)

(*s: type [[Token.t]] *)
type t =
  | Spaces | Newline | EOF

  (* letter or '=' or '!' for commands. ex: 'p' *)
  | Char of char

  (* start of address tokens. ex: "1,3" => [Int 1; Comma; Int 3] *)
  | Int of int
  | Dot | Dollar
  | Comma
  | Plus | Minus
  (*s: [[Token.t]] other cases *)
  | Slash of string | Question of string
  (*x: [[Token.t]] other cases *)
  | Mark of char
  (*x: [[Token.t]] other cases *)
  | Caret
  (*x: [[Token.t]] other cases *)
  | Semicolon
  (*e: [[Token.t]] other cases *)
(*e: type [[Token.t]] *)
[@@deriving show]
(*e: Token.ml *)
