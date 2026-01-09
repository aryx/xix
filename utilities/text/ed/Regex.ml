(*s: Regex.ml *)

(* alt: Re.t *)
(*s: type [[Regex.t]] *)
type t = Str.regexp
(*e: type [[Regex.t]] *)

(*s: function [[Regex.match_str]] *)
let match_str (re : t) (str: string) : bool =
  (* old: Str.string_match re line 0, but we need unanchored search *)
  try
    Str.search_forward re str 0 |> ignore;
    true
   with Not_found -> false
(*e: function [[Regex.match_str]] *)
(*e: Regex.ml *)
