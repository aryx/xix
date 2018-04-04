
(* The list must contain at least one element *)
type pattern = P of pattern_elem list
  and pattern_elem =
    | PStr of string
    | PPercent

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

(* internals *)

val check_pattern: 
  pattern -> unit
