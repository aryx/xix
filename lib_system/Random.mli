(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*              Damien Doligez, projet Para, INRIA Rocquencourt        *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id: random.mli,v 1.4 1997/03/31 15:51:43 doligez Exp $ *)

(* Module [Random]: pseudo-random number generator *)

val init : int -> unit
  (* Initialize the generator, using the argument as a seed.
     The same seed will always yield the same sequence of numbers. *)
val full_init : int array -> unit
  (* Same as [init] but takes more data as seed. *)

val bits : unit -> int
  (* Return 30 random bits in a nonnegative integer. *)
val int : int -> int
  (* [Random.int bound] returns a random integer between 0 (inclusive)
     and [bound] (exclusive).  [bound] must be more than 0 and less
     than $2^{30}$. *)
val float : float -> float
  (* [Random.float bound] returns a random floating-point number
     between 0 (inclusive) and [bound] (exclusive).  If [bound] is
     negative, the result is negative.  If [bound] is 0, the result
     is 0. *)
