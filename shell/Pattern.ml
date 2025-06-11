(*s: shell/Pattern.ml *)

(*s: type [[Pattern.pattern]] *)
type pattern = string
(*e: type [[Pattern.pattern]] *)

(*s: function [[Pattern.match_str]] *)
(* todo: handle [ * ?  *)
let match_str s1 s2 =
  s1 = s2
(*e: function [[Pattern.match_str]] *)
(*e: shell/Pattern.ml *)
