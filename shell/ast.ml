
(* less: W of word_element list  
 *  where word_element = Star | Question | Bracket | Str of string
 * elements: word_element list;
 * quoted: bool;
 *)
type word = string

(* separated by spaces *)
type words = word list

(* less: mv in runtime? *)
type varname = string
type value = words
type var = { 
  mutable v: value option;
  (* less: changed: bool *)
}

type redirection = 
  | RWrite
  | RRead
  | RAppend
  (* less: RHere *)

type line = unit

