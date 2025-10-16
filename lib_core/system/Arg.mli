(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)


(* Module [Arg]: parsing of command line arguments *)

(* This module provides a general mechanism for extracting options and
   arguments from the command line to the program.
*)

(* Syntax of command lines:
    A keyword is a character string starting with a [-].
    An option is a keyword alone or followed by an argument.
    The types of keywords are: [Unit], [Bool], [Set], [Clear],
    [String], [Set_string], [Int], [Set_int], [Float], and [Set_float].
    [Unit], [Set] and [Clear] keywords take no argument. 
    Every other keyword takes the following word on the command line
    as argument.
    Arguments not preceded by a keyword are called anonymous arguments.
*)

(*  Examples ([cmd] is assumed to be the command name):
-   [cmd -flag           ](a unit option)
-   [cmd -int 1          ](an int option with argument [1])
-   [cmd -string foobar  ](a string option with argument ["foobar"])
-   [cmd -float 12.34    ](a float option with argument [12.34])
-   [cmd a b c           ](three anonymous arguments: ["a"], ["b"], and ["c"])
*)

type spec =
  | Unit of (unit -> unit)     (* Call the function with unit argument *)
  | Bool of (bool -> unit)     (* Call the function with a bool argument *)
  | Set of bool ref            (* Set the reference to true *)
  | Clear of bool ref          (* Set the reference to false *)
  | String of (string -> unit) (* Call the function with a string argument *)
  | Set_string of string ref   (* Set the reference to the string argument *)
  | Int of (int -> unit)       (* Call the function with an int argument *)
  | Set_int of int ref         (* Set the reference to the int argument *)
  | Float of (float -> unit)   (* Call the function with a float argument *)
  | Set_float of float ref     (* Set the reference to the float argument *)
        (* The concrete type describing the behavior associated
           with a keyword. *)


(** The concrete type describing the behavior associated
   with a keyword. *)

type key = string
type doc = string
type usage_msg = string
type anon_fun = (string -> unit)

val parse : (string * spec * string) list -> (string -> unit) -> string -> unit
(*
    [parse speclist anonfun usage_msg] parses the command line.
    [speclist] is a list of triples [(key, spec, doc)].
    [key] is the option keyword, it must start with a ['-'] character.
    [spec] gives the option type and the function to call when this option
    is found on the command line.
    [doc] is a one-line description of this option.
    [anonfun] is called on anonymous arguments.
    The functions in [spec] and [anonfun] are called in the same order
    as their arguments appear on the command line.

    If an error occurs, [parse] exits the program, after printing an error
    message as follows:
-   The reason for the error: unknown option, invalid or missing argument, etc.
-   [usage_msg]
-   The list of options, each followed by the corresponding [doc] string.

    For the user to be able to specify anonymous arguments starting with a
    [-], include for example [("--", String anonfun, doc)] in [speclist].

    By default, [parse] recognizes a unit option [-help], which will
    display [usage_msg] and the list of options, and exit the program.
    You can override this behaviour by specifying your own [-help]
    option in [speclist].
*)

exception Bad of string
(** Functions in [spec] or [anon_fun] can raise [Arg.Bad] with an error
    message to reject invalid arguments.
    [Arg.Bad] is also raised by {!Arg.parse_argv} in case of an error. *)

exception Help of string
(** Raised by [Arg.parse_argv] when the user asks for help. *)


(*
     Functions in [spec] or [anonfun] can raise [Bad] with an error
     message to reject invalid arguments.
*)

val usage: (string * spec * string) list -> string -> unit
(*
    [usage speclist usage_msg]
    [speclist] and [usage_msg] are the same as for [parse].  [usage]
    prints the same error message that [parse] prints in case of error.
*)

val current: int ref
(*
    Position (in [Sys.argv]) of the argument being processed.  You can
    change this value, e.g. to force [parse] to skip some arguments.
*)

val align: (key * spec * doc) list -> (key * spec * doc) list
(** Align the documentation strings by inserting spaces at the first alignment
    separator (tab or, if tab is not found, space), according to the length of
    the keyword.  Use a alignment separator as the first character in a doc
    string if you want to align the whole string.  The doc strings corresponding
    to [Symbol] arguments are aligned on the next line.
    @param limit options with keyword and message longer than [limit] will not
    be used to compute the alignment. *)

val parse_argv : string array ->
  (key * spec * doc) list -> anon_fun -> usage_msg -> unit
(** [Arg.parse_argv ~current args speclist anon_fun usage_msg] parses
  the array [args] as if it were the command line.  It uses and updates
  the value of [~current] (if given), or {!Arg.current}.  You must set
  it before calling [parse_argv].  The initial value of [current]
  is the index of the program name (argument 0) in the array.
  If an error occurs, [Arg.parse_argv] raises {!Arg.Bad} with
  the error message as argument.  If option [-help] or [--help] is
  given, [Arg.parse_argv] raises {!Arg.Help} with the help message
  as argument.
*)
