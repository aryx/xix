(*s: Ast.ml *)
(* Copyright 2016, 2025 Yoann Padioleau, see copyright.txt *)

(*s: type [[Ast.value]] *)
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
  | Dollar of value
  | List of values
  (*s: [[Ast.value]] other cases *)
  | Count of value (* $#foo *)
  | Index of value * values (* $foo(...) *)
  (*x: [[Ast.value]] other cases *)
  (* this causes the value and cmd types to be mutually recursive. Uses $IFS *)
  | CommandOutput of cmd_sequence
  (*x: [[Ast.value]] other cases *)
  | Stringify of value (* $"foo " *)
  (*x: [[Ast.value]] other cases *)
  (* ^ distributes over lists *)
  | Concat of value * value
  (*e: [[Ast.value]] other cases *)
(*e: type [[Ast.value]] *)
(*s: type [[Ast.values]] *)
(* separated by spaces *)
and values = value list
(*e: type [[Ast.values]] *)

(*s: type [[Ast.cmd]] *)
and cmd =
  | EmptyCommand

  (* Base *)
  | Simple of value * values
  | Pipe of cmd * cmd (* less: lfd, rfd option *)
  | Async of cmd

  (* Redirections *)
  | Redir of cmd * redirection
  (*s: [[Ast.cmd]] other redirection cases *)
  | Dup of cmd * redirection_kind * int * int (* >[1=2] *)
  (*e: [[Ast.cmd]] other redirection cases *)

  (* expressions *)
  | And of cmd * cmd
  | Or of cmd * cmd
  | Not of cmd
  (* can also run the program 'test' for other comparisons ('[' in bash) *)
  | Match of value * values

  (* stmts *)
  | If of cmd_sequence * cmd
  (* Note that you can not put a 'cmd option' in If instead of IfNot below. 
   * rc has to process 'if(...) cmd\n' now! It can not wait for an else.
   *)
  | IfNot of cmd
  | While of cmd_sequence * cmd
  (*s: [[Ast.cmd]] other statement cases *)
  | Switch of value * cmd_sequence
  (*x: [[Ast.cmd]] other statement cases *)
  | ForIn of value * values * cmd
  (* less: could desugar as ForIn value $* *)
  | For of value * cmd
  (*x: [[Ast.cmd]] other statement cases *)
  | Compound of cmd_sequence
  (*e: [[Ast.cmd]] other statement cases *)

  (* definitions *)
  (* can do x=a; but also $x=b ! 
   * less: could have AssignGlobal and AssignLocal of ... cmd
   *)
  | Assign of value * value * cmd (* can be EmptyCommand *)
  | Fn of value * cmd_sequence
  (*s: [[Ast.cmd]] other definition cases *)
  | DelFn of value
  (*e: [[Ast.cmd]] other definition cases *)
(*e: type [[Ast.cmd]] *)
(*s: type [[Ast.cmd_sequence]] *)
  and cmd_sequence = cmd list
(*e: type [[Ast.cmd_sequence]] *)

(* todo: RDup does not have a filename? so push value inside RWrite of value*)
(*s: type [[Ast.redirection]] *)
  and redirection = redirection_kind * value (* the filename *)
(*e: type [[Ast.redirection]] *)
(*s: type [[Ast.redirection_kind]] *)
    and redirection_kind = 
      | RWrite (* > *)
      | RRead  (* < *)
      | RAppend (* > > *)
      (*s: [[Ast.redirection_kind]] other cases *)
      (* less: RHere *) (* < < *)
      | RDup of int * int (* >[x=y] *)
      (*e: [[Ast.redirection_kind]] other cases *)
(*e: type [[Ast.redirection_kind]] *)

(*s: type [[Ast.line]] *)
(* None when reads EOF *)
type line = cmd_sequence option
(*e: type [[Ast.line]] *)

(* less: type any = Line of line | ... so easier dumper *)
(*e: Ast.ml *)
