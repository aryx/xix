{
(* Copyright 2016 Yoann Padioleau, see copyright.txt *)
open Common

open Ast_cpp
module L = Location_cpp

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
 *  - allow only spaces before the newline; we do not skip any character
 *    (who does put garbage there?)
 *)

let error s =
  raise  (L.Error (spf "Lexical error in cpp: %s" s, !L.line))

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
      { space_or_comment_and_newline lexbuf; Include (file, false)  }
  | "include" space* '<' ([^'>''\n']+ as file) '>'
      { space_or_comment_and_newline lexbuf; Include(file, true) }
  | "include" { error "syntax in #include" }


  | "define" space+ (sym as s1) '(' [^')']* as s2 ')'
      { 
        let xs = Str.split (Str.regexp "[ \t]*,[ \t]*") s2 in

        (* check if identifier or "..." for last one *)
        let params, varargs = 
          let rec aux xs =
            match xs with
            | [] -> [], false
            (* todo: __VA_ARGS__ *)
            | ["..."] -> [], true
            | "..."::xs -> error "... should be the last parameter of a macro"
            | x::xs ->
               if x =~ "[A-Za-z_][A-Za-z_0-9]*" 
               then let (params, bool) = aux xs in x::params, bool
               else error (spf "wrong syntax for macro parameter: %s" x)
          in
          aux xs
        in
        let body = define_body s1 (Common2.index_list_1 params) lexbuf in
        Define (s1, Some (params, varargs), Some body)
      }

  | "define" space+ (sym as s1) space+
      { 
        let body = define_body s1 [] lexbuf in 
        Define (s1, None, Some body)
      }

  | "define" space+ (sym as s1)
      { space_or_comment_and_newline lexbuf; Define (s1, None, None) }

  | "define" { error "syntax in #define" }

  (* stricter: require comment-or-space-only after sym also for #undef *)
  | "undef" space+ (sym as s)
      { space_or_comment_and_newline lexbuf; Undef s }
  | "undef" { error "syntax in #undef" } 


  | "ifdef"
  | "ifndef"
  | "else"

  | "endif" { Endif }

  (* stricter: I impose a filename (with no quote in name, like original?) *)
  | "#line" space+ (digit+ as s1) space* ('"' ([^'"']* as s2) '"')
      { Line (int_of_string s1, s2) }
  | "#line" { error "syntax in #line" }


  | "pragma" space+ "lib" space* '"' ([^'"''\n']+ as file) '"'
      { space_or_comment_and_newline lexbuf; Pragma("lib", [file]) }
  | "pragma" space+ "src" space* '"' ([^'"''\n']+ as file) '"'
      { space_or_comment_and_newline lexbuf; Pragma("src", [file]) }

  | "pragma" space+ "varargck" space* "argpos" [^'\n']+
      { space_or_comment_and_newline lexbuf; Pragma("varargck", ["TODO"]) }
  | "pragma" space+ "varargck" space* "type" [^'\n']+
      { space_or_comment_and_newline lexbuf; Pragma("varargck", ["TODO"]) }
  | "pragma" space+ "varargck" space* "flag" [^'\n']+
      { space_or_comment_and_newline lexbuf; Pragma("varargck", ["TODO"]) }

  | "pragma"  { error "syntax in #pragma" }

  | sym { error (spf "unknown #: %s" (Lexing.lexeme lexbuf)) }

(*****************************************************************************)
(* Macro body *)
(*****************************************************************************)
and define_body macro params = parse
  | sym as s  
     {
      try 
        let i = List.assoc s params in
        (* safe to use # for a special mark since C code can not
         * use this symbol since it is reserved by cpp
         *)
        spf "#%d" i ^ define_body macro params lexbuf
      with Not_found ->
        s ^ define_body macro params lexbuf
     }
  | [^ '\n' '_''a'-'z''A'-'Z' '\'' '"' '\\' '/']+ as s 
      { s ^ define_body macro params lexbuf }

  | "'" { let s = define_body_char macro params lexbuf in 
          "'" ^ s ^ define_body macro params lexbuf }
  | '"' { let s = define_body_string macro params lexbuf in 
          "\"" ^ s ^ define_body macro params lexbuf }

  | "//" [^'\n']* { define_body macro params lexbuf }
  | "/*"          { comment_star lexbuf; define_body macro params lexbuf }

  | '/' { "/" ^ define_body macro params lexbuf }

  | "\\" "\n" { incr Location_cpp.line; " " ^ define_body macro params lexbuf }
  | '\\' { "\\" ^ define_body macro params lexbuf }
  (* end of macro *)
  | '\n' { incr Location_cpp.line; "" }

  | eof  { error (spf "eof in macro %s" macro) }

(*****************************************************************************)
(* Strings and characters *)
(*****************************************************************************)

and define_body_char macro params = parse
  (* no need for stringify! substitute also in strings *)
  | sym as s  
     { try let i = List.assoc s params in
        spf "#%d" i ^ define_body_char macro params lexbuf
      with Not_found -> s ^ define_body_char macro params lexbuf
     }
  | [^ '\n' '_' 'a'-'z''A'-'Z' '\'' '\\']+ as s 
      { s ^ define_body_char macro params lexbuf }
  | '\\' _ 
      { let s = Lexing.lexeme lexbuf in s^define_body_char macro params lexbuf }
  | "'" { "'" }
  | '\n' { error (spf "newline in character in macro %s" macro) }
  | eof  { error (spf "eof in macro %s" macro) }

and define_body_string macro params = parse
  (* no need for stringify! substitute also in strings *)
  | sym as s  
     { try let i = List.assoc s params in
        spf "#%d" i ^ define_body_string macro params lexbuf
      with Not_found -> s ^ define_body_string macro params lexbuf
     }
  | [^ '\n' '_' 'a'-'z''A'-'Z' '"' '\\']+ as s 
      { s ^ define_body_string macro params lexbuf }
  | '\\' _ 
      { let s=Lexing.lexeme lexbuf in s^define_body_string macro params lexbuf }
  | '"' { "\"" }
  | '\n' { error (spf "newline in string in macro %s" macro) }
  | eof  { error (spf "eof in macro %s" macro) }


(*****************************************************************************)
(* Macro arguments *)
(*****************************************************************************)

(*****************************************************************************)
(* Comment *)
(*****************************************************************************)

and space_or_comment_and_newline = parse
  | space+        { space_or_comment_and_newline lexbuf }
  | "//" [^'\n']* { space_or_comment_and_newline lexbuf }
  | "/*"          { comment_star lexbuf; space_or_comment_and_newline lexbuf }
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
(* Skipping, for ifdefs *)
(*****************************************************************************)
