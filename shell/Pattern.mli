(*s: Pattern.mli *)
(* todo: Glob char *)
(*s: type [[Pattern.pattern]] *)
type pattern = string
(*e: type [[Pattern.pattern]] *)

(*s: signature [[Pattern.match_str]] *)
val match_str : string -> pattern -> bool
(*e: signature [[Pattern.match_str]] *)
(*e: Pattern.mli *)
