(*s: Address.ml *)
(*s: type [[Address.t]] *)
(* An "address" is a way to specify a line number symbolically or literally *)
type t =
  | Current (* '.' *)
  | Last    (* '$' *)
  | Line of int (* <n> *)
  | Mark of char (* \a *)
  | SearchFwd of string (* /.../ *)
  | SearchBwd of string (* ?...? *)
  | Relative of t * int (* -, +, ^ *)
[@@deriving show]
(*e: type [[Address.t]] *)
(*s: type [[Address.range]] *)
(* What is parsed before a command. For instance 1,3p will be parsed as
 * { addr1 = Some (Line 1); addr2 = Line 3; given = true; set_dot = false}.
 *)
type range = {
  addr1 : t option;
  addr2 : t;
  given : bool;
  set_dot : bool;
}
[@@deriving show]
(*e: type [[Address.range]] *)
(*e: Address.ml *)
