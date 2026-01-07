
type t =
  | Current (* '.' *)
  | Last    (* '$' *)
  | Line of int (* <n> *)
  | Mark of char (* \a *)
  | SearchFwd of string (* /.../ *)
  | SearchBwd of string (* ?...? *)
  | Relative of t * int (* -, +, ^ *)
[@@deriving show]

type range = {
  addr1 : t option;
  addr2 : t;
  given : bool;
  set_dot : bool;
}
[@@deriving show]


val parse_range: Parser.state -> range

val eval_range: Env.t -> range -> Env.lineno * Env.lineno
