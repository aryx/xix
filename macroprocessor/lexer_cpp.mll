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
 *    (even though some people like to do  '#  ifdef', when have nested ifdefs)
 *  - single '#' are not converted in #endif
 *    (who does use that?)
 *  - only spaces before the newline; we do not skip any character
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

(* pre: 'token' assumes the '#' has already been consumed *)
rule token = parse

  (* note that filenames containing double quotes are not supported *)
  | "include" space* '"' ([^ '"' '\n']+ as file) '"'
      { space_or_comment_and_newline lexbuf; 
        Include (file, false)  }
  | "include" space* '<' ([^ '>' '\n']+ as file) '>'
      { space_or_comment_and_newline lexbuf; 
        Include(file, true) }
  | "include" { error "syntax in #include" }

  (* Macro definition part 1 *)
  | "define" space+ (sym as s1) '(' ([^')']* as s2) ')'
      { let xs = Str.split (Str.regexp "[ \t]*,[ \t]*") s2 in
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
      { let body = define_body s1 [] lexbuf in 
        Define (s1, None, Some body)  }
  (* if do '#define FOO+' then? should return syntax error *)
  | "define" space+ (sym as s1)
      { space_or_comment_and_newline lexbuf; 
        Define (s1, None, None) }

  | "define" { error "syntax in #define" }

  (* stricter: require space_or_comment-only after sym also for #undef *)
  | "undef" space+ (sym as s)
      { space_or_comment_and_newline lexbuf; 
        Undef s }
  | "undef" { error "syntax in #undef" } 


  | "ifdef" space+ (sym as s) 
      { space_or_comment_and_newline lexbuf;
        Ifdef s }
  | "ifndef" space+ (sym as s) 
      { space_or_comment_and_newline lexbuf;
        Ifndef s }
  | "ifdef" { error "syntax in #ifdef" } 
  | "ifndef" { error "syntax in #ifndef" } 

  | "else"  
      { space_or_comment_and_newline lexbuf; 
        Else }
  | "endif" 
      { space_or_comment_and_newline lexbuf; 
        Endif }

  (* stricter: I impose a filename (with no quote in name, like original?) *)
  | "#line" space+ (digit+ as s1) space* ('"' ([^'"']* as s2) '"')
      { Line (int_of_string s1, s2) }
  | "#line" { error "syntax in #line" }


  | "pragma" space+ "lib" space* '"' ([^'"''\n']+ as file) '"'
      { space_or_comment_and_newline lexbuf; 
        Pragma("lib", [file]) }
  | "pragma" space+ "src" space* '"' ([^'"''\n']+ as file) '"'
      { space_or_comment_and_newline lexbuf; 
        Pragma("src", [file]) }

  | "pragma" space+ "varargck" space* "argpos" [^'\n']+
      { space_or_comment_and_newline lexbuf; 
        Pragma("varargck", ["TODO"]) }
  | "pragma" space+ "varargck" space* "type" [^'\n']+
      { space_or_comment_and_newline lexbuf; 
        Pragma("varargck", ["TODO"]) }
  | "pragma" space+ "varargck" space* "flag" [^'\n']+
      { space_or_comment_and_newline lexbuf; 
        Pragma("varargck", ["TODO"]) }
  | "pragma"  { error "syntax in #pragma" }

  | sym { error (spf "unknown #: %s" (Lexing.lexeme lexbuf)) }

(*****************************************************************************)
(* Macro definition part 2, the body *)
(*****************************************************************************)
and define_body name params = parse
  | sym as s  
     { try 
        let i = List.assoc s params in
        (* safe to use # for a special mark since C code can not
         * use this symbol since it is reserved by cpp
         *)
        spf "#%d" i ^ define_body name params lexbuf
      with Not_found ->
        s ^ define_body name params lexbuf
     }

  | [^ '\n' '_''a'-'z''A'-'Z' '\'' '"' '\\' '/' '#']+ as s 
      { s ^ define_body name params lexbuf }

  (* end of macro *)
  | '\n' { incr Location_cpp.line; "" }

  (* special cases *)
  | "'" 
      { let s = define_body_char name params lexbuf in 
        "'" ^ s ^ define_body name params lexbuf }
  | '"' 
      { let s = define_body_string name params lexbuf in 
         "\"" ^ s ^ define_body name params lexbuf }

  | "//" [^'\n']* { define_body name params lexbuf }
  | "/*"          { comment_star lexbuf; define_body name params lexbuf }
  | '/' { "/" ^ define_body name params lexbuf }

  (* we escape single '#' by doubling it (classic) *)
  | '#' { "##" ^ define_body name params lexbuf }

  (* could do " " ^ but that is not what 5c does *)
  | "\\" "\n" { incr Location_cpp.line; define_body name params lexbuf }
  | '\\' { "\\" ^ define_body name params lexbuf }

  | eof  { error (spf "eof in macro %s" name) }

(*****************************************************************************)
(* Strings and characters (part1) *)
(*****************************************************************************)

(* Diff with string/character handling in lexer.mll for C?
 *  - need to recognize macro parameter (yes 5c does that! no need stringify)
 *  - need to escape '#' to ##
 *  - do not care about precise parsing of \\, octal, escaped char,
 *    just return the string
*)

and define_body_char name params = parse
  (* no need for stringify! substitute also in strings *)
  | sym as s  
     { try let i = List.assoc s params in
        spf "#%d" i ^ define_body_char name params lexbuf
       with Not_found -> s ^ define_body_char name params lexbuf
     }
  | [^ '\n' '_' 'a'-'z''A'-'Z' '\'' '\\' '#']+ as s 
      { s ^ define_body_char name params lexbuf }
  (* todo: what if escaped newline here? allowed??? *)
  | '\\' _ 
      { let s = Lexing.lexeme lexbuf in s^define_body_char name params lexbuf }

  (* escape # to disambiguate with use of # to reference a parameter *)
  | '#' { "##" ^ define_body_char name params lexbuf }

  | "'" { "'" }
  | '\n' { error (spf "newline in character in macro %s" name) }
  | eof  { error (spf "eof in macro %s" name) }

(* less: could factorize *)
and define_body_string name params = parse
  (* no need for stringify! substitute also in strings *)
  | sym as s  
     { try let i = List.assoc s params in
        spf "#%d" i ^ define_body_string name params lexbuf
       with Not_found -> s ^ define_body_string name params lexbuf
     }
  | [^ '\n' '_' 'a'-'z''A'-'Z' '"' '\\' '#']+ as s 
      { s ^ define_body_string name params lexbuf }
  | '\\' _ 
      { let s=Lexing.lexeme lexbuf in s^define_body_string name params lexbuf }
  | '#' { "##" ^ define_body_string name params lexbuf }
  | '"' { "\"" }
  | '\n' { error (spf "newline in string in macro %s" name) }
  | eof  { error (spf "eof in macro %s" name) }


(*****************************************************************************)
(* Macro use *)
(*****************************************************************************)

(* Extracting the arguments. 
 * Note that as opposed to the other rules in this file, here
 * we do not parse a directive; we parse C code used as arguments to
 * a macro. We still use ocamllex for that because it is convenient.
 *)
and macro_arguments = parse
 | space* "(" 
     { macro_args 0 "" [] lexbuf |> List.rev }
 (* stricter: better error message *)
 | _ as c { error (spf "was expecting a '(' not %c for macro arguments" c) }
 | eof    { error "was expecting a '(', not an eof for macro arguments" }

and macro_args depth str args = parse
 | ")" 
     { if depth = 0 
       then str::args
       else error "TODO: macro_args )"
     }

 | [^ '\'' '"' '/' ',' '\n' '(' ')']+ 
     { macro_args depth (str ^ Lexing.lexeme lexbuf) args lexbuf }

 | "," 
     { if depth = 0 
       (* todo: if reached varargs! *)
       then macro_args 0 "" (str::args) lexbuf 
       else error "TODO: macro_args , depth <>0 "
     }

 | "("  { macro_args (depth+1) (str^"(") args lexbuf }

 (* special cases *)
 | "'" { error "TODO: macro_args quote" }
 | '"' { error "TODO: macro_args double quote" }

 | "//" { error "TODO: macro_args comment1" }
 | "/*" { error "TODO: macro_args comment2" }
 | "/"  { macro_args depth     (str^"/") args lexbuf }

 | "\n" { incr Location_cpp.line; macro_args depth     (str^" ") args lexbuf }

 | _ as c { error (spf "unrecognized character: '%c'" c) }
 (* stricter: better error message *)
 | eof { error "eof in macro arguments" }

(* Substituting the arguments.
 * Note that the lexbuf here will be different. It will correspond
 * to the string of the body of a macro.
 *)
and subst_args_in_macro_body name args = parse
 | [^ '\n' '#']+ as s { s ^ subst_args_in_macro_body name args lexbuf }
 | "##" { "#" ^ subst_args_in_macro_body name args lexbuf }
 | "#" (digit+ as s) 
     { let i = int_of_string s in
       try 
         let arg = List.nth args (i - 1) in
         arg ^ subst_args_in_macro_body name args lexbuf
       with Failure _ -> 
         (* stricter: better error message *)
         error (spf "could not find argument %d in macro of %s" i name)
     }
 (* the escaped newlines should have been removed *)
 | "\n" { failwith "impossible: newline in macro body" }
 | eof { "" }

(*****************************************************************************)
(* Comment *)
(*****************************************************************************)

and space_or_comment_and_newline = parse
  | space+        { space_or_comment_and_newline lexbuf }
  | "\n"          { incr Location_cpp.line }

  | "//" [^'\n']* { space_or_comment_and_newline lexbuf }
  | "/*"          { comment_star lexbuf; space_or_comment_and_newline lexbuf }

  (* stricter: new error message *)
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
