{
(* Copyright 2016 Yoann Padioleau, see copyright.txt *)
open Common

open Preprocessor

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Limitations compared to cpp:
 * 
 * stricter: 
 *  - no space allowed before the # keyword
 *    (but some people like to do  #    ifdef, when have nested ifdefs)
 *  - empty # are not converted in #endif
 *    (who does use that?)
 *  - allow only space before the newline, we do not skip any character
 *    (who does put garbage there?)
 *)

(* todo: use Hist to report error at the right place, 
 * throw Preprocessor.Error then? *)
let error s =
  failwith (spf "Lexical error in cpp: %s (line %d)" s !Globals.line)

}

(*****************************************************************************)
(* Regexps aliases *)
(*****************************************************************************)
let space  = [' ''\t']
let letter = ['a'-'z''A'-'Z']
let digit  = ['0'-'9']

let sym = (letter | '_') (letter | digit | '_')*

(*****************************************************************************)
(* Main rule *)
(*****************************************************************************)

rule token = parse
  | "ifdef"
  | "ifndef"
  | "else"

  | "endif" { Endif }

  (* stricter: I impose a filename (with no quote in name, hmm) 
   * less: normalize? realpath? 
   * dup: lexer_asm5.mll
   *)
  | "line"
(*
  | "#line" space+ (digit+ as s1) space* ('"' ([^'"']* as s2) '"') 
      { TSharpLine (int_of_string s1, s2) }
  | "#line" { error "syntax in #line" }
*)

  | "define"

  | "undef" space+ sym [^'\n']*
      { }
  | "undef"
      { error "syntax in #undef" } 

  | "include" space+ '"' ([^'"''\n']+ as file) '"'
      { comment_and_newline lexbuf;  }
  | "include" space+ '<' ([^'>''\n']+ as file) '>'
      { comment_and_newline lexbuf; }
  | "include"
      { error "syntax in #include" }

  | "pragma"

  | sym { error (spf "unknown #: %s" (Lexing.lexeme lexbuf)) }

(*****************************************************************************)
(* Comment *)
(*****************************************************************************)

and comment_and_newline = parse
  | space+        { comment_and_newline lexbuf }
  | "//" [^'\n']* { comment_and_newline lexbuf }
  | "/*"          { comment_star lexbuf; comment_and_newline lexbuf }
  | "\n"          { incr Globals.line }
  (* pad: new error message *)
  | c             { error (spf "unexpected character %c after directive" c) }
  | eof           { error "expected newline, not EOF" }

and comment_star = parse
  | "*/"          { }
  | [^ '*' '\n']+ { comment_star lexbuf }
  | '*'           { comment_star lexbuf }
  | '\n'          { error "comment across newline" }
  | eof           { error "eof in comment" }

(*****************************************************************************)
(* skip *)
(*****************************************************************************)
