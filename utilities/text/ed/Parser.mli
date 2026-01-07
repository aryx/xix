(*s: Parser.mli *)
open Common

(*s: type [[Parser.state]] *)
type state = {
  stdin: Lexing_.lexbuf;
  (* for inserting "virtual" commands to process before stdin *)
  mutable globp: Lexing_.lexbuf option;
  mutable lookahead : Token.t option;
}
(*e: type [[Parser.state]] *)
[@@deriving show]

(*s: signature [[Parser.init]] *)
val init: <Cap.stdin; ..> -> state
(*e: signature [[Parser.init]] *)

(*s: signature [[Parser.peek]] *)
val peek : state -> Token.t
(*e: signature [[Parser.peek]] *)
(*s: signature [[Parser.consume]] *)
val consume: state -> Token.t
(*e: signature [[Parser.consume]] *)

(*s: signature [[Parser.parse_address_range]] *)
(*e: signature [[Parser.parse_address_range]] *)

(*s: signature [[Parser.was_expecting]] *)
(* internals that are used outside for now *)
val was_expecting: string -> 'a
(*e: signature [[Parser.was_expecting]] *)
(*s: signature [[Parser.was_expecting_but_got]] *)
val was_expecting_but_got: string -> Token.t -> 'a
(*e: signature [[Parser.was_expecting_but_got]] *)
(*e: Parser.mli *)
