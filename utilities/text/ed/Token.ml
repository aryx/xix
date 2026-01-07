(*s: Token.ml *)
(* The tokens *)

(*s: type [[Token.t]] *)
type t =
  | Spaces | Newline | EOF

  (* letter or '=' *)
  | Char of char

  (* start of address tokens *)
  | Int of int
  | Dot | Dollar
  | Comma | Semicolon
  | Plus | Minus | Caret
  | Slash of string | Question of string
  | Mark of char
(*e: type [[Token.t]] *)
[@@deriving show]
(*e: Token.ml *)
