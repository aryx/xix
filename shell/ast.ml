
type word = 
  (* the string can contain * ? [ special char
   * less: the should be preceded by \001
   * 
   * less: W of word_element list  
   *  where word_element = Star | Question | Bracket | Str of string
   * elements: word_element list;
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

type cmd =
  | EmptyCommand
  | Simple of word * words
  | Compound of cmd list

  | Redir of redirection * cmd
  | Pipe of cmd * cmd

  | And of cmd * cmd
  | Or of cmd * cmd
  | Not of cmd

  | Match of word * words

  | If of cmd list * cmd
  | IfNot of cmd

  | While of cmd list * cmd
  | Switch of word * cmd list

  | ForIn of word * words * cmd
  | For of word * cmd

  | Fn of word * cmd list
  | DelFn of word

type line = unit

