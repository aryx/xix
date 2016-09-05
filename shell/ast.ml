
(* 
 * less: W of word_element list  
 *  where word_element = Star | Question | Bracket | Str of string
 * elements: word_element list;
 *)
type word = 
  (* the string can contain * ? [ special char
   * less: the should be preceded by \001
   *)
  | Word of string * bool (* quoted *)

  | Dollar of word
  | Count of word

  | List of words
  | Index of word * words
  (* ^ is distributive on lists *)
  | Concat of word * word

(* separated by spaces *)
and words = word list


type redirection = 
  | RWrite
  | RRead
  | RAppend
  (* less: RHere *)

type line = unit

