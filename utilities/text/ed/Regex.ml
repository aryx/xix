
type t = Str.regexp

(* val match_str: Regex.t -> string -> bool *)

let match_str (re : t) (str: string) : bool =
  (* old: Str.string_match re line 0, but we need unanchored search *)
  try
    Str.search_forward re str 0 |> ignore;
    true
   with Not_found -> false
