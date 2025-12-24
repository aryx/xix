open Common

type state = {
  stdin: Lexing_.lexbuf;
  (* for inserting "virtual" commands to process before stdin *)
  mutable globp: Lexing_.lexbuf option;
  mutable lookahead : Token.t option;
}
[@@deriving show]

val init: <Cap.stdin; ..> -> state

val peek : state -> Token.t
val consume: state -> Token.t

val parse_address_range: state -> Address.range

(* internals that are used outside for now *)
val was_expecting: string -> 'a
val was_expecting_but_got: string -> Token.t -> 'a
