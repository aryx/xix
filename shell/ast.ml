
(* rc does not use types. Anyway there is no integer, no boolean, no float.
 * The only value in RC is the list of strings. Even a single
 * string is really a list with one element.
 *)
type value = 
  (* the string can contain * ? [ special char
   * less: the should be preceded by \001
   * so even a single word can expand to a list of strings.
   * 
   * less: W of word_element list  
   *  where word_element = Star | Question | Bracket | Str of string
   *)
  | Word of string * bool (* quoted *)
  | List of value

  | Dollar of value

  | Count of value
  | Index of value * values

  (* ^ distributes over lists *)
  | Concat of value * value

(* separated by spaces *)
and values = value list

type redirection = 
  | RWrite (* > *)
  | RRead  (* < *)
  | RAppend (* >> *)
  (* less: RHere *) (* << *)

type cmd =
  | EmptyCommand

  (* Base *)

  | Simple of value * values

  (* IO *)

  | Redir of redirection * cmd
  | Pipe of cmd * cmd

  (* expressions *)

  | And of cmd * cmd
  | Or of cmd * cmd
  | Not of cmd

  | Match of value * values

  (* stmts *)

  | If of cmd_sequence * cmd
  | IfNot of cmd

  | While of cmd_sequence * cmd
  | Switch of value * cmd_sequence

  | ForIn of value * values * cmd
  | For of value * cmd

  | Compound of cmd_sequence

  (* definitions *)

  | Fn of value * cmd_sequence
  | DelFn of value
  (* can do x=a; but also $x=b ! *)
  | Assign of value * value

and cmd_sequence =
  | Async of cmd * cmd_sequence
  | Seq   of cmd * cmd_sequence
  | Last  of cmd

(* None when reads EOF *)
type line = cmd_sequence option
