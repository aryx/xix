
(* rc does not use types; there is no integer, no boolean, no float.
 * The only value in rc is the list of strings. Even a single
 * string is really a list with one element.
 *)
type value = 
  (* The string can contain the * ? [ special characters.
   * So, even a single word can expand to a list of strings.
   * less: they should be preceded by \001
   * less: W of word_elt list and word_elt = Star | Question | ...Str of string
   *)
  | Word of string * bool (* quoted *)
  | List of values
  (* this causes the value and cmd types to be mutually recursive. Uses $IFS *)
  | CommandOutput of cmd_sequence

  | Dollar of value

  | Count of value (* $#foo *)
  | Index of value * values (* $foo(...) *)

  (* ^ distributes over lists *)
  | Concat of value * value

  | Stringify of value (* $"foo " *)

(* separated by spaces *)
and values = value list


and cmd =
  | EmptyCommand

  (* Base *)

  | Simple of value * values
  | Pipe of cmd * cmd (* less: lfd, rfd option *)
  | Async of cmd

  (* Redirections *)

  | Redir of cmd * redirection
  | Dup of cmd * redirection_kind * int * int (* >[1=2] *)

  (* expressions *)

  | And of cmd * cmd
  | Or of cmd * cmd
  | Not of cmd

  | Match of value * values
  (* can also run the program 'test' for other comparisons *)

  (* stmts *)

  | If of cmd_sequence * cmd
  (* Note that you can not put a 'cmd option' in If instead of IfNot below. 
   * rc has to process 'if(...) cmd\n' now! It can not wait for an else.
   *)
  | IfNot of cmd

  | While of cmd_sequence * cmd
  | Switch of value * cmd_sequence

  | ForIn of value * values * cmd
  (* less: could desugar as ForIn value $* *)
  | For of value * cmd

  | Compound of cmd_sequence

  (* definitions *)

  | Fn of value * cmd_sequence
  | DelFn of value
  (* can do x=a; but also $x=b ! 
   * less: could have AssignGlobal and AssignLocal of ... cmd
   *)
  | Assign of value * value * cmd (* can be EmptyCommand *)

  and cmd_sequence = cmd list

  (* todo: RDup does not have a filename? so push value inside RWrite of value*)
  and redirection = redirection_kind * value (* the filename *)

    and redirection_kind = 
      | RWrite (* > *)
      | RRead  (* < *)
      | RAppend (* > > *)
      (* less: RHere *) (* < < *)
      | RDup of int * int (* >[x=y] *)
     (* with tarzan *)


(* None when reads EOF *)
type line = cmd_sequence option

(* less: type any = Line of line | ... so easier dumper *)

 (* with tarzan *)
