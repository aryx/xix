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

(* $Id: std_exit.ml,v 1.3 1996/10/09 11:15:13 xleroy Exp $ *)

(* Ensure that [at_exit] functions are called at the end of every program *)

let _ = do_at_exit()
