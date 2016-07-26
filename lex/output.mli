(*s: lex/output.mli *)
(*s: copyright ocamllex *)
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
(*e: copyright ocamllex *)
(*s: signature Output.output_lexdef *)
(* Output the DFA tables and its entry points *)

val output_lexdef:
      in_channel -> out_channel ->
      Ast.location (* header *) ->
      Compact.lex_tables ->
      Lexgen.automata_entry list ->
      Ast.location (* trailer *) ->
      unit
(*e: signature Output.output_lexdef *)

val output_lexdef_simple:
      in_channel -> out_channel ->
      Ast.location (* header *) ->
      Lexgen.automata_entry list * Lexgen.automata_matrix ->
      Ast.location (* trailer *) ->
      unit


(*e: lex/output.mli *)
