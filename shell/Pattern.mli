(*s: Pattern.mli *)
(* todo: Glob char *)
(*s: type [[Pattern.pattern (Pattern.mli)]] *)
type pattern = string
(*e: type [[Pattern.pattern (Pattern.mli)]] *)

(*s: signature [[Pattern.match_str]] *)
val match_str : string -> pattern -> bool
(*e: signature [[Pattern.match_str]] *)
(*e: Pattern.mli *)
