(*s: Percent.mli *)
(* The list must contain at least one element *)
(*s: type [[Percent.pattern (Percent.mli)]] *)
type pattern = P of pattern_elem list
(*e: type [[Percent.pattern (Percent.mli)]] *)
(*s: type [[Percent.pattern_elem (Percent.mli)]] *)
and pattern_elem = PStr of string | PPercent
(*e: type [[Percent.pattern_elem (Percent.mli)]] *)

(*s: exception [[Percent.TooManyPercents (Percent.mli)]] *)
exception TooManyPercents
(*e: exception [[Percent.TooManyPercents (Percent.mli)]] *)
(*s: exception [[Percent.PercentNotFound (Percent.mli)]] *)
exception PercentNotFound
(*e: exception [[Percent.PercentNotFound (Percent.mli)]] *)

(*s: signature [[Percent.match_]] *)
(* return the possible stem *)
val match_ : pattern -> string -> string option
(*e: signature [[Percent.match_]] *)
(*s: signature [[Percent.subst]] *)
val subst : pattern -> string (* stem *) -> string
(*e: signature [[Percent.subst]] *)

(*s: signature [[Percent.match_and_subst]] *)
(* print a warning if not match *)
val match_and_subst :
  pattern (* pattern *) -> pattern (* subst *) -> string (* src *) -> string
(*e: signature [[Percent.match_and_subst]] *)

(*s: signature [[Percent.check_pattern]] *)
(* internals *)

val check_pattern : pattern -> unit
(*e: signature [[Percent.check_pattern]] *)
(*e: Percent.mli *)
