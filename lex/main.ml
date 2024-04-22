(*s: lex/main.ml *)
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
(* The lexer generator. Command-line parsing. *)

open Stdcompat (* for |> *)
open Ast
open Lexgen
open Output

(*s: function [[Main.main]] *)
let main () =
  (*s: [[Main.main()]] print lex usage if wrong number of arguments *)
  if Array.length Sys.argv != 2 then begin
    prerr_endline "Usage: ocamllex <input file>";
    exit 2
  end;
  (*e: [[Main.main()]] print lex usage if wrong number of arguments *)
  let source_name = Sys.argv.(1) in
  let dest_name =
    if Filename.check_suffix source_name ".mll" 
    then Filename.chop_suffix source_name ".mll" ^ ".ml"
    else source_name ^ ".ml" 
  in
  let ic = open_in source_name in
  let oc = open_out dest_name in
  let lexbuf = Lexing.from_channel ic in

  (* parsing *)
  let def =
    try
      Parser.lexer_definition Lexer.main lexbuf
    with exn ->
      close_out oc;
      Sys.remove dest_name;
       (*s: [[Main.main()]] report error exn *)
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
       (*e: [[Main.main()]] report error exn *)
      exit 2 
  in
  (* compiling *)
  let (entries, transitions) = Lexgen.make_dfa def in

(* CONFIG
  Output.output_lexdef_simple ic oc 
    def.header (entries, transitions) def.trailer;
*)
  (* optimizing *)
  let tables = Compact.compact_tables transitions in
  (* generating *)
  Output.output_lexdef ic oc def.header tables entries def.trailer;
  close_in ic;
  close_out oc
(*e: function [[Main.main]] *)

(*s: toplevel [[Main._1]] *)
let _ = 
  Printexc.catch main (); 
  exit 0
(*e: toplevel [[Main._1]] *)

(*e: lex/main.ml *)
