type t =
  | Spaces
  | Newline
  | EOF

  | Letter of char
  | Int of int
  | String of string

  | Comma
  (* TODO: TPlus, TMinus, TPlusPlus, ... *)
[@@deriving show]
