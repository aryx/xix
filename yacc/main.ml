(*s: yacc/main.ml *)
(*s: copyright ocamlyacc *)
(* Yoann Padioleau
 *
 * Copyright (C) 2015 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file license.txt.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * license.txt for more details.
 *)
(*e: copyright ocamlyacc *)
open Ast

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* An OCaml port of yacc.
 *   
 * The original yacc is written in old C. ocamlyacc in the OCaml
 * distribution is actually also written in C.
 *
 * todo:
 *  - handle priorities, precedences
 *  - EBNF support!
 *)

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

(*s: function [[Main.main]](yacc) *)
let main () =

  if Array.length Sys.argv != 2 then begin
    prerr_endline "Usage: ocamlyacc <input file>";
    exit 2
  end;

  let source_name = Sys.argv.(1) in

  let dest_name =
    if Filename.check_suffix source_name ".mly" 
    then Filename.chop_suffix source_name ".mly" ^ ".ml"
    else source_name ^ ".ml" 
  in
  let ic = open_in source_name in
  let lexbuf = Lexing.from_channel ic in

  (* parsing *)
  let def =
    try
      Parser.parser_definition Lexer.main lexbuf
    with exn ->
      Sys.remove dest_name;
       (match exn with
         Parsing.Parse_error ->
           prerr_string "Syntax error around char ";
           prerr_int (Lexing.lexeme_start lexbuf);
           prerr_endline "."
       | Lexer.Lexical_error s ->
           prerr_string "Lexical error around char ";
           prerr_int (Lexing.lexeme_start lexbuf);
           prerr_string ": ";
           prerr_string s;
           prerr_endline "."
       | _ -> raise exn
       );
      exit 2 
  in
  let env = Lr0.mk_env_augmented_grammar (Ast.start_symbol def) def.grm  in
  let automaton = Lr0.canonical_lr0_automaton env in
  Dump.dump_lr0_automaton env automaton;
  
  let (first, eps) = First_follow.compute_first def.grm in
  let follow = First_follow.compute_follow env (first, eps) in
  let tables = Slr.lr_tables env automaton follow in
  Dump.dump_lrtables env tables;

  let oc = open_out dest_name in
  Output.output_parser def env tables ic oc;
  close_out oc;
  ()
(*e: function [[Main.main]](yacc) *)

(*s: toplevel [[Main._1]](yacc) *)
let _ = 
(*
  Tests.test_lr0 ();
  Tests.test_first_follow ();
  Tests.test_slr ();
  Tests.test_lr_engine ();
*)
  (*Printexc.catch*) main (); 
  exit 0
(*e: toplevel [[Main._1]](yacc) *)
(*e: yacc/main.ml *)
