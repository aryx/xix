(*s: Address.mli *)

(*s: type [[Address.t]] *)
(* An "address" is a way to specify a line number symbolically or literally *)
type t =
  | Current (* '.' *)
  | Last    (* '$' *)
  | Line of int (* <n> *)
  | Relative of t * int (* -, +, ^ *)
  (*s: [[Address.t]] other cases *)
  | SearchFwd of string (* /.../ *)
  | SearchBwd of string (* ?...? *)
  (*x: [[Address.t]] other cases *)
  | Mark of char (* \a *)
  (*e: [[Address.t]] other cases *)
(*e: type [[Address.t]] *)
(*s: type [[Address.range]] *)
(* What is parsed before a command. For instance 1,3 will be parsed as
 * { addr1 = Some (Line 1); addr2 = Line 3; given = true; set_dot = false}.
 *)
type range = {
  addr1 : t option;
  addr2 : t;
  given : bool;
  (*s: [[Address.range]] other fields *)
  set_dot : bool;
  (*e: [[Address.range]] other fields *)
}
(*e: type [[Address.range]] *)

(*s: signature [[Address.parse_range]] *)
val parse_range: Parser.state -> range
(*e: signature [[Address.parse_range]] *)
(*s: signature [[Address.eval_range]] *)
val eval_range: Env.t -> range -> Env.lineno * Env.lineno
(*e: signature [[Address.eval_range]] *)
(*e: Address.mli *)
