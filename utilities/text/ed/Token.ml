(* The tokens *)

type t =
  | Spaces | Newline | EOF
  (* letter or '=' *)
  | Char of char
  | Int of int

  | Dot | Dollar
  | Comma | Semicolon
  
  | Plus | Minus | Caret

  | Slash of string | Question of string
  | Mark of char
[@@deriving show]
