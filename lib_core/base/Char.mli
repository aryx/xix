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


(* Module [Char]: character operations *)

external code: char -> int = "%identity"
        (* Return the ASCII code of the argument. *)
val chr: int -> char
        (* Return the character with the given ASCII code.
           Raise [Invalid_argument "Char.chr"] if the argument is
           outside the range 0--255. *)
val escaped : char -> string
        (* Return a string representing the given character,
           with special characters escaped following the lexical conventions
           of Objective Caml. *)
val lowercase: char -> char
val uppercase: char -> char
        (* Convert the given character to its equivalent lowercase or
           uppercase character, respectively. *)
(*--*)

external unsafe_chr: int -> char = "%identity"

type t = char
(** An alias for the type of characters. *)

val compare: t -> t -> int
(** The comparison function for characters, with the same specification as
    {!Stdlib.compare}.  Along with the type [t], this function [compare]
    allows the module [Char] to be passed as argument to the functors
    {!Set.Make} and {!Map.Make}. *)

val equal: t -> t -> bool
(** The equal function for chars.
    @since 4.03.0 *)
