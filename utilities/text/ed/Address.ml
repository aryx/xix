
(* An "address" is a way to specific a line number symbolically or literally *)
type t =
  | Current (* '.' *)
  | Last    (* '$' *)
  | Line of int
  | Mark of char (* \a *)
  | SearchFwd of string (* /.../ *)
  | SearchBwd of string (* ?...? *)
  | Relative of t * int (* -, +, ^ *)
[@@deriving show]

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
