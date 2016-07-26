{
(* Copyright 2016 Yoann Padioleau, see copyright.txt *)
open Common

open Parser

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Limitations compared to mk:
 *  - escaped newlines not handled in comments
 *  - does not support `{} rc syntax
 *  - does not handle unicode (use ulex?)
 *)

let error s =
  failwith (spf "%s:%d: Lexical error %s" !Globals.file !Globals.line s)

(* pad: hack around ocamllex to emulate the yyless() of flex. The semantic
 * is not exactly the same than yyless(), so I use yyback() instead.
 * http://my.safaribooksonline.com/book/programming/flex/9780596805418/a-reference-for-flex-specifications/yyless
 *)
let yyback n lexbuf =
  lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_curr_pos - n;
  let currp = lexbuf.Lexing.lex_curr_p in
  lexbuf.Lexing.lex_curr_p <- { currp with
    Lexing.pos_cnum = currp.Lexing.pos_cnum - n;
  }

let loc () = 
  { Ast.file = !Globals.file;
    Ast.line = !Globals.line;
  }
  

(* state *)
type state = 
  | Start
  (* once we started to parse a rule, the next newline will start a recipe *)
  | AfterColon
  (* the lexing rules are different in a recipe; we do not parse rc's input *)
  | InRecipe

(* see also parse.ml and code using that global *)
let state = ref Start

}

(*****************************************************************************)
(* Regexps aliases *)
(*****************************************************************************)
let space = [' ''\t']
let letter = ['a'-'z''A'-'Z''_']
let number = ['0'-'9']

(* less: WORDCHR = !utfrune("!\"#$%&'()*+,-./:;<=>?@[\\]^`{|}~", (r) *)
let ident = letter (letter | number)*


(*****************************************************************************)
(* Main rule *)
(*****************************************************************************)
rule token = parse

  (* ----------------------------------------------------------------------- *)
  (* Spacing/comments *)
  (* ----------------------------------------------------------------------- *)
  | space+        { TSpace (Lexing.lexeme lexbuf) }

  (* less: does not handle escaped newline in comment *)
  | '#' [^ '\n']* { token lexbuf }

  | '\\' '\n'     { incr Globals.line; TSpace (Lexing.lexeme lexbuf) }

  | '\n' { incr Globals.line;
           state := if !state = AfterColon then InRecipe else Start;
           TNewline }
  (* ----------------------------------------------------------------------- *)
  (* Symbols *)
  (* ----------------------------------------------------------------------- *)

  | ':' { state := AfterColon; TColon (loc()) }
  | '=' { TEq (loc()) }
  | '<' { TInf (loc()) }
  | '%' { TPercent }

  (* ----------------------------------------------------------------------- *)
  (* Variables *)
  (* ----------------------------------------------------------------------- *)

  (* todo: handle ${ } *)
  | '$' (ident as s) { TVar s }
  | '$'              { error "missing variable name" }

  (* ----------------------------------------------------------------------- *)
  (* Quoted strings *)
  (* ----------------------------------------------------------------------- *)
  | "'" { TQuoted (quote lexbuf) }

  (* stricter: does not leading allow space *)
  | "`" { TBackquoted (backquote lexbuf) }

  (* ----------------------------------------------------------------------- *)
  (* Regular stuff *)
  (* ----------------------------------------------------------------------- *)
  | [^'\'' ':' '=' '<' '$' '%' '\n' '\\' '#' ' ' '\t']+ 
      { TOther (Lexing.lexeme lexbuf) }

  (* ----------------------------------------------------------------------- *)
  | eof { EOF }
  | _   { error "unrecognized character" }

(*****************************************************************************)
(* Rule quote *)
(*****************************************************************************)
(* opti? could use Buffer *)
and quote = parse
  | "'"                  { "" }
  | "''"                 { "'" ^ quote lexbuf }

  | '\\' '\n'            { incr Globals.line; " " ^ quote lexbuf }
  (* new: instead of "missing closing '"  *)
  | '\n' { error "newline in quoted string" }

  | [^ '\\' '\'' '\n']+  { let x = Lexing.lexeme lexbuf in x ^ quote lexbuf }

  (* new: instead of "missing closing '"  *)
  | eof  { error "end of file in quoted string" }
  | _    { error "missing closing '" }

(*****************************************************************************)
(* Rule backquote *)
(*****************************************************************************)

and backquote = parse
  | "`"                  { "" }

  | '\\' '\n'            { incr Globals.line; " " ^ backquote lexbuf }
  (* new: instead of "missing closing '"  *)
  | '\n' { error "newline in quoted string" }

  | [^ '\\' '\'' '`' '\n']+ 
      { let x = Lexing.lexeme lexbuf in x ^ backquote lexbuf}
  | "'" { let s = quote lexbuf in s ^ backquote lexbuf }


  (* new: instead of "missing closing `"  *)
  | eof  { error "end of file in backquoted string" }
  | _    { error "missing closing `" }


(*****************************************************************************)
(* Rule recipe *)
(*****************************************************************************)
and recipe = parse
  | ('#'   [^'\n']*) as s '\n' { incr Globals.line; TLineRecipe s }
  | space ([^'\n']*) as s '\n' { incr Globals.line; TLineRecipe s }
  | [^ '#' ' ' '\t']           { state := Start; yyback 1 lexbuf; TEndRecipe }

  | eof { EOF }
  | _ { error "unrecognized character in recipe" }
