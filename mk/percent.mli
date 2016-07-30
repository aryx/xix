
type pattern_elem =
  | PStr of string
  | PPercent
type pattern = P of pattern_elem list

exception TooManyPercents
exception PercentNotFound

(* return the possible stem *)
val match_: 
  pattern -> string -> string option
  
val subst:
  pattern -> string (* stem *) -> string

(* print a warning if not match *)
val match_and_subst:
  pattern (* pattern *) -> pattern (* subst *) -> string (* src *) -> string
