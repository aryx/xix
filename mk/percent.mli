
exception TooManyPercents
exception PercentNotFound

(* return the possible stem *)
val match_: 
  Ast.word (* the pattern, only String and Percent *) -> string 
  -> string option
  
val subst:
  Ast.word (* the subst, only String and Percent *) -> string (* stem *)
  -> string

(* print a warning if not match *)
val match_and_subst:
  Ast.word (* pattern *) -> Ast.word (* subst *) -> string (* src *) 
  -> string
