
exception TooManyPercents
exception PercentNotFound

(* return the possible stem *)
val match_: 
  Rules.pattern -> string -> string option
  
val subst:
  Rules.pattern -> string (* stem *) -> string

(* print a warning if not match *)
val match_and_subst:
  Rules.pattern (* pattern *) -> Rules.pattern (* subst *) -> string (* src *) 
  -> string
