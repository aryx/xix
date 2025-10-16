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

(* $Id: char.mli,v 1.8 1997/06/20 12:50:11 doligez Exp $ *)

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
