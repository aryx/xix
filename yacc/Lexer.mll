(*s: yacc/Lexer.mll *)
{
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

open Ast (* Location *)
open Parser

exception Lexical_error of string

(*s: Lexer helper functions and globals(yacc) *)
let comment_depth = ref 0
(*x: Lexer helper functions and globals(yacc) *)
let brace_depth = ref 0
(*e: Lexer helper functions and globals(yacc) *)
}

(*s: rule Lexer.main(yacc) *)
rule main = parse
  (*s: [[Lexer.main()]] space case (yacc) *)
  | [' ' '\010' '\013' '\009' '\012' ] + 
      { main lexbuf }
  (*e: [[Lexer.main()]] space case (yacc) *)
  (*s: [[Lexer.main()]] comment case (yacc) *)
  | "(*" 
      { comment_depth := 1;
        comment lexbuf;
        main lexbuf }
  (*e: [[Lexer.main()]] comment case (yacc) *)
  (*s: [[Lexer.main()]] keyword or identifier case (yacc) *)
  (* terminals, uppercase *)
  | ['A'-'Z' ] ['A'-'Z' 'a'-'z' '\'' '_' '0'-'9'] *
      { TTerm (T (Lexing.lexeme lexbuf)) }
  (* nonterminals, lowercase *)
  | ['a'-'z' ] ['A'-'Z' 'a'-'z' '\'' '_' '0'-'9'] *
      { TNonterm (NT (Lexing.lexeme lexbuf)) }
  (* directives, % prefixed *)
  | '%' ['A'-'Z' 'a'-'z'] ['A'-'Z' 'a'-'z' '\'' '_' '0'-'9'] *
      { match Lexing.lexeme lexbuf with
        | "%token" -> Ttoken
        | "%prec" -> Tprec
        | "%start" -> Tstart
        | "%type" -> Ttype
        | s -> failwith ("Unknown directive: " ^ s)
      }
  (*e: [[Lexer.main()]] keyword or identifier case (yacc) *)
  (*s: [[Lexer.main()]] action case (yacc) *)
  (* actions and header/trailer *)
  | '{' 
      { let n1 = Lexing.lexeme_end lexbuf in
        brace_depth := 1;
        let n2 = action lexbuf in
        TAction(Location(n1, n2)) }
  (*e: [[Lexer.main()]] action case (yacc) *)
  (*s: [[Lexer.main()]] operator cases (yacc) *)
  | ':'  { TColon }
  | '|'  { TOr }
  | ';'  { TSemicolon }
  (*e: [[Lexer.main()]] operator cases (yacc) *)
  (*s: [[Lexer.main()]] type case (yacc) *)
  (* for types *)
  | '<'  { TAngle (angle lexbuf) }
  (*e: [[Lexer.main()]] type case (yacc) *)
  (*s: [[Lexer.main()]] backward compatible cases (yacc) *)
  (* to be backward compatible with ocamlyacc *)
  | "%%" { main lexbuf }
  | "%{" 
      { let n1 = Lexing.lexeme_end lexbuf in
        brace_depth := 1;
        let n2 = action2 lexbuf in
        TAction(Location(n1, n2)) }
  | "/*" { comment2 lexbuf; main lexbuf }
  (*e: [[Lexer.main()]] backward compatible cases (yacc) *)
| eof  { TEOF }
| _
    { raise(Lexical_error
             ("illegal character " ^ String.escaped(Lexing.lexeme lexbuf))) }
(*e: rule Lexer.main(yacc) *)

(*s: rule Lexer.action(yacc) *)
(* TODO: handle $x *)
and action = parse
| '{' 
    { incr brace_depth;
      action lexbuf }
| '}' 
    { decr brace_depth;
      if !brace_depth == 0 
      then Lexing.lexeme_start lexbuf 
      else action lexbuf }
| "(*" 
    { comment_depth := 1;
      comment lexbuf;
      action lexbuf }

| eof { raise (Lexical_error "unterminated action") }
| _   { action lexbuf }
(*e: rule Lexer.action(yacc) *)

(*s: rule Lexer.comment(yacc) *)
and comment = parse
| "(*" 
    { incr comment_depth; 
      comment lexbuf }
| "*)" 
    { decr comment_depth;
      if !comment_depth == 0 
      then () 
      else comment lexbuf }

| eof { raise(Lexical_error "unterminated comment") }
| _ { comment lexbuf }
(*e: rule Lexer.comment(yacc) *)

(*s: rule Lexer.angle(yacc) *)
and angle = parse
| '>' { "" }
| eof { raise(Lexical_error "unterminated type") }
| [^'>']+ { let s = Lexing.lexeme lexbuf in s ^ angle lexbuf }
| _ { let s = Lexing.lexeme lexbuf in s ^ angle lexbuf }
(*e: rule Lexer.angle(yacc) *)

(*s: backward compatible lexing rules(yacc) *)
(* to be backward compatible with ocamlyacc *)
and action2 = parse
| '{' 
    { incr brace_depth;
      action2 lexbuf }
| "%}" 
    { decr brace_depth;
      if !brace_depth == 0 
      then Lexing.lexeme_start lexbuf 
      else action2 lexbuf }
| "}" 
    { decr brace_depth;
      if !brace_depth == 0 
      then Lexing.lexeme_start lexbuf 
      else action2 lexbuf }
| "(*" 
    { comment_depth := 1;
      comment lexbuf;
      action2 lexbuf }

| eof { raise (Lexical_error "unterminated action") }
| _   { action2 lexbuf }

(* to be backward compatible with ocamlyacc *)
and comment2 = parse
| "*/" { () }
| ['*''/']+ { comment2 lexbuf }
| eof { raise (Lexical_error "unterminated C comment") }
| _   { comment2 lexbuf }
(*e: backward compatible lexing rules(yacc) *)
(*e: yacc/Lexer.mll *)
