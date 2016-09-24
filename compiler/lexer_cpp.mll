{
(* Copyright 2016 Yoann Padioleau, see copyright.txt *)
open Common

open Ast_cpp

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Limitations compared to cpp:
 *  - no support for unicode
 * 
 * stricter: 
 *  - no space allowed between '#' and the directive
 *    (but some people like to do  '#    ifdef', when have nested ifdefs)
 *  - empty # are not converted in #endif
 *    (who does use that?)
 *  - allow only space before the newline; we do not skip any character
 *    (who does put garbage there?)
 *)

let error s =
  raise  (Error.Error (spf "Lexical error in cpp: %s" s, !Location_cpp.line))

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

  | "include" space* '"' ([^'"''\n']+ as file) '"'
      { comment_and_newline lexbuf; Include (file, false)  }
  | "include" space* '<' ([^'>''\n']+ as file) '>'
      { comment_and_newline lexbuf; Include(file, true) }
  | "include" { error "syntax in #include" }


  | "define" space+ (sym as s1) '(' [^')']* as s2 ')'
      { 
        let xs = Str.split (Str.regexp "[ \t]*,[ \t]*") s2 in
        (* check if identifier or "..." for last one? *)
        let params = raise Todo in
        let varargs = raise Todo in

        let body = define_body params lexbuf in
        Define (s1, Some (params, varargs), Some body)
      }

  | "define" space+ (sym as s1) space+
      { 
        let body = define_body [] lexbuf in 
        Define (s1, None, Some body)
      }

  | "define" space+ (sym as s1)
      { comment_and_newline lexbuf; Define (s1, None, None) }

  | "define" { error "syntax in #define" }

  | "undef" space+ (sym as s)
      { comment_and_newline lexbuf; Undef s }
  | "undef" { error "syntax in #undef" } 


  | "ifdef"
  | "ifndef"
  | "else"

  | "endif" { Endif }

  (* stricter: I impose a filename (with no quote in name, like original?) 
   * less: normalize? realpath? 
   * dup: lexer_asm5.mll
   *)
  | "#line" space+ (digit+ as s1) space* ('"' ([^'"']* as s2) '"')
      { Line (int_of_string s1, s2) }
  | "#line" { error "syntax in #line" }


  | "pragma"  { error "syntax in #pragma" }

  | sym { error (spf "unknown #: %s" (Lexing.lexeme lexbuf)) }

(*****************************************************************************)
(* Macro body *)
(*****************************************************************************)
and define_body params = parse
  | sym as s  
     {
      try 
        let i = List.assoc s params in
        spf "#%d" i ^ define_body params lexbuf
      with Not_found ->
        s ^ define_body params lexbuf
     }
  | [^'\n' '_' 'a'-'z''A'-'Z']+ as s { s ^ define_body params lexbuf }
  | '\n' { incr Location_cpp.line; "" }
  | eof  { "" }

(*****************************************************************************)
(* Comment *)
(*****************************************************************************)

and comment_and_newline = parse
  | space+        { comment_and_newline lexbuf }
  | "//" [^'\n']* { comment_and_newline lexbuf }
  | "/*"          { comment_star lexbuf; comment_and_newline lexbuf }
  | "\n"          { incr Location_cpp.line }
  (* pad: new error message *)
  | _ as c        { error (spf "unexpected character %c after directive" c) }
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
