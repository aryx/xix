(*s: Parser.mli *)

(*s: type [[Parser.state]] *)
type state = {
  stdin: Lexing.lexbuf;
  mutable lookahead : Token.t option;
  (*s: [[Parser.state]] other fields *)
  (* for inserting "virtual" commands to process before stdin *)
  mutable globp: Lexing.lexbuf option;
  (*e: [[Parser.state]] other fields *)
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

(*s: signature [[Parser.was_expecting]] *)
(* internals that are used outside for now *)
val was_expecting: string -> 'a
(*e: signature [[Parser.was_expecting]] *)
(*s: signature [[Parser.was_expecting_but_got]] *)
val was_expecting_but_got: string -> Token.t -> 'a
(*e: signature [[Parser.was_expecting_but_got]] *)
(*e: Parser.mli *)
