(*s: Percent.mli *)
(*s: type [[Percent.pattern]] *)
(* The list must contain at least one element *)
type pattern = P of pattern_elem list
(*e: type [[Percent.pattern]] *)
(*s: type [[Percent.pattern_elem]] *)
and pattern_elem =
  | PStr of string
  | PPercent
(*e: type [[Percent.pattern_elem]] *)

(*s: exception [[Percent.TooManyPercents]] *)
exception TooManyPercents
(*e: exception [[Percent.TooManyPercents]] *)
(*s: exception [[Percent.PercentNotFound]] *)
exception PercentNotFound
(*e: exception [[Percent.PercentNotFound]] *)

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

(* internals *)

(*s: signature [[Percent.check_pattern]] *)
val check_pattern : pattern -> unit
(*e: signature [[Percent.check_pattern]] *)
(*e: Percent.mli *)
