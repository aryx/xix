(*s: lex/output.ml *)
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
(* Output the DFA tables and its entry points *)

open Printf
open Ast
open Lexgen
open Compact

(*s: constant [[Output.copy_buffer]] *)
(* To copy the ML code fragments *)

let copy_buffer = Bytes.create 1024
(*e: constant [[Output.copy_buffer]] *)

(*s: function [[Output.copy_chunk]] *)
let copy_chunk ic oc (Location(start,stop)) =
  seek_in ic start;
  let n = ref (stop - start) in
  while !n > 0 do
    let m = input ic copy_buffer 0 (min !n 1024) in
    output oc copy_buffer 0 m;
    n := !n - m
  done
(*e: function [[Output.copy_chunk]] *)

(*s: function [[Output.output_byte]] *)
(* To output an array of short ints, encoded as a string *)

let output_byte oc b =
  output_char oc '\\';
  output_char oc (Char.chr(48 + b / 100));
  output_char oc (Char.chr(48 + (b / 10) mod 10));
  output_char oc (Char.chr(48 + b mod 10))
(*e: function [[Output.output_byte]] *)

(*s: function [[Output.output_array]] *)
let output_array oc v =
  output_string oc "   \"";
  for i = 0 to Array.length v - 1 do
    output_byte oc (v.(i) land 0xFF);
    output_byte oc ((v.(i) asr 8) land 0xFF);
    if i land 7 = 7 then output_string oc "\\\n    "
  done;
  output_string oc "\""
(*e: function [[Output.output_array]] *)

(*s: function [[Output.output_tables]] *)
(* Output the tables *)

let output_tables oc tbl =
  output_string oc "let lex_tables = {\n";
  fprintf oc "  Lexing.lex_base = \n%a;\n" output_array tbl.tbl_base;
  fprintf oc "  Lexing.lex_backtrk = \n%a;\n" output_array tbl.tbl_backtrk;
  fprintf oc "  Lexing.lex_default = \n%a;\n" output_array tbl.tbl_default;
  fprintf oc "  Lexing.lex_trans = \n%a;\n" output_array tbl.tbl_trans;
  fprintf oc "  Lexing.lex_check = \n%a\n" output_array tbl.tbl_check;
  output_string oc "}\n\n"
(*e: function [[Output.output_tables]] *)

(*s: function [[Output.output_entry]] *)
(* Output the entries *)

let output_entry ic oc e =
  fprintf oc "%s lexbuf = %s_rec lexbuf %d\n"
          e.auto_name e.auto_name e.auto_initial_state;
  fprintf oc "and %s_rec lexbuf state =\n" e.auto_name;
  fprintf oc "  match Lexing.engine lex_tables state lexbuf with\n    ";
  let first = ref true in
  e.auto_actions |> List.iter (fun (num, loc_action) ->
      if !first 
      then first := false 
      else fprintf oc "  | ";
      fprintf oc "%d -> (" num;
      copy_chunk ic oc loc_action;
      fprintf oc ")\n"
  );
  fprintf oc "  | n -> lexbuf.Lexing.refill_buff lexbuf; %s_rec lexbuf n\n\n"
          e.auto_name
(*e: function [[Output.output_entry]] *)

(*s: function [[Output.output_lexdef]] *)
(* Main output function *)

let output_lexdef ic oc header tables entry_points trailer =
  (*s: [[Output.output_lexdef()]] print statistics *)
  Printf.printf "%d states, %d transitions, table size %d bytes\n"
    (Array.length tables.tbl_base)
    (Array.length tables.tbl_trans)
    (2 * (Array.length tables.tbl_base + Array.length tables.tbl_backtrk +
          Array.length tables.tbl_default + Array.length tables.tbl_trans +
          Array.length tables.tbl_check));
  flush stdout;
  (*e: [[Output.output_lexdef()]] print statistics *)
  copy_chunk ic oc header;
  output_tables oc tables;
  (*s: [[Output.output_lexdef()]] generate entry points *)
  (match entry_points with
    [] -> ()
  | entry1 :: entries ->
      output_string oc "let rec "; 
      output_entry ic oc entry1;
      entries |> List.iter (fun e -> 
        output_string oc "and "; 
        output_entry ic oc e
      )
  );
  (*e: [[Output.output_lexdef()]] generate entry points *)
  copy_chunk ic oc trailer
(*e: function [[Output.output_lexdef]] *)

(*****************************************************************************)
(* Simpler version *)
(*****************************************************************************)

let debug = ref true

(* 1- Generating the actions *)

let output_action ic oc (i,act) =
  output_string oc ("action_" ^ string_of_int i ^ " lexbuf = (\n");
  if !debug 
  then output_string oc (" log \"action_" ^ string_of_int i ^ "\";\n");
  copy_chunk ic oc act;
  output_string oc ")\nand ";
  ()

(* 2- Generating the states *)

let states = ref ([||] : Lexgen.automata_matrix)

let enumerate_vect v =
  let rec enum env pos =
    if pos >= Array.length v 
    then env 
    else
      try
        let pl = List.assoc v.(pos) env in
          pl := pos :: !pl; enum env (succ pos)
        with Not_found ->
          enum ((v.(pos), ref [pos]) :: env) (succ pos) 
  in
  List.sort
    (fun (_e1, pl1) (_e2, pl2) -> compare (List.length !pl1)  (List.length !pl2))
    (enum [] 0)

let output_move oc = function
    Backtrack ->
      if !debug
      then output_string oc "log \"backtrack\"; ";
      output_string oc "backtrack lexbuf"
  | Goto dest ->
      match !states.(dest) with
        Perform act_num ->
          output_string oc ("action_" ^ string_of_int act_num ^ " lexbuf")
      | _ ->
          (* Many states are just Perform so this explains why there is some
           * big jumps in the generated files from e.g. state_3 to state_9
           * without any intermediate state_4 function; it's because state 4
           * was a Perform.
           *)
          output_string oc ("state_" ^ string_of_int dest ^ " lexbuf")

let output_char_for_read oc = function
    '\''  -> output_string oc "\\'"
  | '\\' -> output_string oc "\\\\"
  | '\n' -> output_string oc "\\n"
  | '\t' -> output_string oc "\\t"
  | c ->
      let n = Char.code c in
      if n >= 32 && n < 127 then
        output_char oc c
      else begin
        output_char oc '\\';
        output_char oc (Char.chr (48 + n / 100));
        output_char oc (Char.chr (48 + (n / 10) mod 10));
        output_char oc (Char.chr (48 + n mod 10))
      end

let rec output_chars oc = function
    [] ->
      failwith "output_chars"
  | [c] ->
      if c <= 255 then begin
      output_string oc "'";
      output_char_for_read oc (Char.chr c);
      output_string oc "'"
      end else output_string oc "'\\000'"
  | c::cl ->
      if c <= 255 then begin
      output_string oc "'";
      output_char_for_read oc (Char.chr c);
      output_string oc "'|";
      output_chars oc cl
      end else output_string oc "'\\000'"

let output_one_trans oc (dest, chars) =
  output_chars oc !chars;
  output_string oc " -> ";
  output_move oc dest;
  output_string oc "\n |  ";
  ()

let output_all_trans oc trans =
  output_string oc "  let c = Lexing.get_next_char lexbuf in\n";
  if !debug
  then output_string oc "  log (\"consuming:\" ^ (Char.escaped c));\n";
  output_string oc "  match c with\n    ";
  match enumerate_vect trans with
    [] ->
      failwith "output_all_trans"
  | (default, _) :: rest ->
      List.iter (output_one_trans oc) rest;
      output_string oc "_ -> ";
      output_move oc default;
      output_string oc "\nand ";
      ()

let output_state oc state_num = function
    Perform _i ->
      ()
  | Shift(what_to_do, moves) ->
      output_string oc
        ("state_"  ^ string_of_int state_num ^ " lexbuf =\n");
      if !debug 
      then output_string oc("  log \"state_" ^string_of_int state_num^ "\";\n");

    (match what_to_do with
      No_remember -> ()
    | Remember i ->
        output_string oc "  lexbuf.lex_last_pos <- lexbuf.lex_curr_pos;\n";
        output_string oc ("  lexbuf.lex_last_action_simple <- Obj.magic action_" ^
                             string_of_int i ^ ";\n")
    );
    output_all_trans oc moves

(* 3- Generating the entry points *)
          
let rec output_entries oc = function
    [] -> failwith "output_entries"
  | entry :: rest ->
      let name = entry.auto_name in
      let state_num = entry.auto_initial_state in
      output_string oc (name ^ " lexbuf =\n");
      if !debug 
      then output_string oc ("log \"" ^ name ^ "\";\n");
      output_string oc "  Lexing.start_lexing lexbuf;\n";
      output_string oc ("  state_" ^ string_of_int state_num ^ " lexbuf\n");
      match rest with
        [] -> output_string oc "\n"; ()
      | _  -> output_string oc "\nand "; output_entries oc rest

(* All together *)

let output_lexdef_simple ic oc header (initial_st, st) trailer =
  print_int (Array.length st); print_string " states, ";
(*  print_int (List.length actions); print_string " actions."; *)
  print_newline();
  (* for the labels *)
  output_string oc "open Lexing\n\n";
  if !debug
  then output_string oc
    "let log x = print_endline (\"LEX: \" ^ x); flush stdout\n";
  copy_chunk ic oc header;
  output_string oc "\nlet rec ";
  states := st;
  initial_st |> List.iter (fun entry ->
    let actions = entry.auto_actions in
    List.iter (output_action ic oc) actions;
    output_string oc "\n";
  );
  output_string oc "\n";
  for i = 0 to Array.length st - 1 do
    output_state oc i st.(i)
  done;
  output_string oc "\n";
  output_entries oc initial_st;
  output_string oc "\n";
  copy_chunk ic oc trailer

(*e: lex/output.ml *)
