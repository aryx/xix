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

(* Facilities for printing exceptions. *)

val to_string : exn -> string
        (* [Printexc.to_string e] returns a string representation of [e]. *)

val print: ('a -> 'b) -> 'a -> 'b
        (* Same as [catch], but re-raise the stray exception after
           printing it, instead of aborting the program. *)

val catch: ('a -> 'b) -> 'a -> 'b
        (* [Printexc.catch fn x] applies [fn] to [x] and returns the result.
           If the evaluation of [fn x] raises any exception, the
           name of the exception is printed on standard error output,
           and the programs aborts with exit code 2.
           Typical use is [Printexc.catch main ()], where [main], with type
           [unit->unit], is the entry point of a standalone program.
           This catches and reports any exception that escapes the program. *)

(* forward port of 3.10.2 *)
val get_backtrace: unit -> string
(** [Printexc.get_backtrace ()] returns a string containing the
    same exception backtrace that [Printexc.print_backtrace] would
    print. *)


