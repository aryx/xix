
type word = 
  (* the string can contain * ? [ special char
   * less: the should be preceded by \001
   * 
   * less: W of word_element list  
   *  where word_element = Star | Question | Bracket | Str of string
   *)
  | Word of string * bool (* quoted *)
  | List of words

  | Dollar of word

  | Count of word
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
  | Compound of cmd_sequence

  | Redir of redirection * cmd
  | Pipe of cmd * cmd

  | And of cmd * cmd
  | Or of cmd * cmd
  | Not of cmd

  | Match of word * words

  | If of cmd_sequence * cmd
  | IfNot of cmd

  | While of cmd_sequence * cmd
  | Switch of word * cmd_sequence

  | ForIn of word * words * cmd
  | For of word * cmd

  | Fn of word * cmd_sequence
  | DelFn of word

and cmd_sequence =
  | Async of cmd * cmd_sequence
  | Seq   of cmd * cmd_sequence
  | Last  of cmd

(* None for EOF *)
type line = cmd_sequence option
