type t =
  | Spaces
  | Newline
  | EOF

  (* letter or '=' *)
  | Char of char
  | Int of int
  | String of string

  | Comma
  (* TODO: TPlus, TMinus, TPlusPlus, ... *)
[@@deriving show]
