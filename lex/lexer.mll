(*s: lex/lexer.mll *)
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
(* The lexical analyzer for lexer definitions. Bootstrapped! *)

{
open Stdcompat (* for Bytes *)
open Ast
open Parser

exception Lexical_error of string

(* Auxiliaries for the lexical analyzer *)
(*s: Lexer helper functions and globals *)
let comment_depth = ref 0
(*x: Lexer helper functions and globals *)
let initial_string_buffer = Bytes.create 256
let string_buff = ref initial_string_buffer
let string_index = ref 0
(*x: Lexer helper functions and globals *)
let reset_string_buffer () =
  string_buff := initial_string_buffer;
  string_index := 0
(*x: Lexer helper functions and globals *)
let get_stored_string () =
  Bytes.sub_string !string_buff 0 !string_index
(*x: Lexer helper functions and globals *)
let store_string_char c =
  if !string_index >= Bytes.length !string_buff then begin
    let new_buff = Bytes.create (Bytes.length !string_buff * 2) in
    Bytes.blit !string_buff 0 new_buff 0 (Bytes.length !string_buff);
    string_buff := new_buff
  end;
  Bytes.set !string_buff !string_index c;
  incr string_index
(*x: Lexer helper functions and globals *)
let char_for_backslash = function
    'n' -> '\n'
  | 't' -> '\t'
  | 'b' -> '\b'
  | 'r' -> '\r'
  | c   -> c
(*x: Lexer helper functions and globals *)
let char_for_decimal_code lexbuf i =
  Char.chr(100 * (Char.code(Lexing.lexeme_char lexbuf i) - 48) +
            10 * (Char.code(Lexing.lexeme_char lexbuf (i+1)) - 48) +
                 (Char.code(Lexing.lexeme_char lexbuf (i+2)) - 48))
(*x: Lexer helper functions and globals *)
let brace_depth = ref 0
(*e: Lexer helper functions and globals *)
}

(*s: rule Lexer.main *)
rule main = parse
  (*s: [[Lexer.main()]] space case *)
    [' ' '\010' '\013' '\009' '\012' ] + 
    { main lexbuf }
  (*e: [[Lexer.main()]] space case *)
  (*s: [[Lexer.main()]] comment case *)
  | "(*" 
    { comment_depth := 1;
      comment lexbuf;
      main lexbuf }
  (*e: [[Lexer.main()]] comment case *)
  (*s: [[Lexer.main()]] keyword or identifier case *)
  | ['A'-'Z' 'a'-'z'] ['A'-'Z' 'a'-'z' '\'' '_' '0'-'9'] *
    { match Lexing.lexeme lexbuf with
      | "rule" -> Trule
      | "parse" -> Tparse
      | "and" -> Tand
      | "eof" -> Teof
      | "let" -> Tlet
      | s -> Tident s 
     }
  (*e: [[Lexer.main()]] keyword or identifier case *)
  (*s: [[Lexer.main()]] string start case *)
  | '"' 
    { reset_string_buffer();
      string lexbuf;
      Tstring(get_stored_string()) }
  (*e: [[Lexer.main()]] string start case *)
  (*s: [[Lexer.main()]] character cases *)
  | "'" [^ '\\'] "'" 
    { Tchar(Char.code(Lexing.lexeme_char lexbuf 1)) }
  | "'" '\\' ['\\' '\'' 'n' 't' 'b' 'r'] "'" 
    { Tchar(Char.code(char_for_backslash (Lexing.lexeme_char lexbuf 2))) }
  | "'" '\\' ['0'-'9'] ['0'-'9'] ['0'-'9'] "'" 
    { Tchar(Char.code(char_for_decimal_code lexbuf 2)) }
  (*e: [[Lexer.main()]] character cases *)
  (*s: [[Lexer.main()]] operator cases *)
  | '*'  { Tstar }
  | '|'  { Tor }
  (*x: [[Lexer.main()]] operator cases *)
  | '?'  { Tmaybe }
  | '+'  { Tplus }
  (*x: [[Lexer.main()]] operator cases *)
  | '('  { Tlparen }
  | ')'  { Trparen }
  (*x: [[Lexer.main()]] operator cases *)
  | '['  { Tlbracket }
  | ']'  { Trbracket }
  | '-'  { Tdash }
  | '^'  { Tcaret }
  (*x: [[Lexer.main()]] operator cases *)
  | '_'  { Tunderscore }
  (*x: [[Lexer.main()]] operator cases *)
  | '='  { Tequal }
  (*e: [[Lexer.main()]] operator cases *)
  (*s: [[Lexer.main()]] action case *)
  | '{' 
    { let n1 = Lexing.lexeme_end lexbuf in
      brace_depth := 1;
      let n2 = action lexbuf in
      Taction(Location(n1, n2)) }
  (*e: [[Lexer.main()]] action case *)
  | eof  { Tend }
  | _
    { raise(Lexical_error
             ("illegal character " ^ String.escaped(Lexing.lexeme lexbuf))) }
(*e: rule Lexer.main *)

(*s: rule Lexer.action *)
and action = parse
    '{' 
    { incr brace_depth;
      action lexbuf }
  | '}' 
    { decr brace_depth;
      if !brace_depth == 0 then Lexing.lexeme_start lexbuf else action lexbuf }
  | "(*" 
    { comment_depth := 1;
      comment lexbuf;
      action lexbuf }
  | eof 
    { raise (Lexical_error "unterminated action") }
  | _ 
    { action lexbuf }
(*e: rule Lexer.action *)

(*s: rule Lexer.string *)
and string = parse
    '"' 
    { () }
  | '\\' [' ' '\010' '\013' '\009' '\026' '\012'] +
    { string lexbuf }
  | '\\' ['\\' '"' 'n' 't' 'b' 'r'] 
    { store_string_char(char_for_backslash(Lexing.lexeme_char lexbuf 1));
      string lexbuf }
  | '\\' ['0'-'9'] ['0'-'9'] ['0'-'9'] 
    { store_string_char(char_for_decimal_code lexbuf 1);
      string lexbuf }
  | eof 
    { raise(Lexical_error "unterminated string") }
  | _ 
    { store_string_char(Lexing.lexeme_char lexbuf 0);
      string lexbuf }
(*e: rule Lexer.string *)
      
(*s: rule Lexer.comment *)
and comment = parse
    "(*" 
    { incr comment_depth; comment lexbuf }
  | "*)" 
    { decr comment_depth;
      if !comment_depth == 0 then () else comment lexbuf }

  | eof 
    { raise(Lexical_error "unterminated comment") }
  | _ 
    { comment lexbuf }
(*e: rule Lexer.comment *)
(*e: lex/lexer.mll *)
