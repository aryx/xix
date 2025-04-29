{
(* Copyright 2016 Yoann Padioleau, see copyright.txt *)
open Common

open Parser

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Limitations compared to mk:
 *  - does not handle unicode (use ulex?)
 *)

let error s =
  failwith (spf "%s:%d: Lexical error, %s" !Globals.file !Globals.line s)

let loc () = 
  { Ast.file = Fpath.v !Globals.file; Ast.line = !Globals.line; }

 
(* lexer state *)
type state = 
  | Start

  (* once we started to parse a rule, the next newline will start a recipe *)
  | AfterColon
  (* the lexing rules are different in a recipe; we do not parse rc's input *)
  | InRecipe

  (* once we started to parse an assign, the second = is like a string *)
  | AfterEq
  (* except inside ${x:...=...} where we still want = to be TEq *)
  | InBrace

(* see also parse.ml and code using that global *)
let state = ref Start

(* A single var is enough since mk does not allow recursivity in braces
 * as in ${x:%${y}x=%.c}. We do not need a stack.
 * *)
let save_state_outside_brace = ref Start

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

}

(*****************************************************************************)
(* Regexps aliases *)
(*****************************************************************************)
let space = [' ''\t']
let letter = ['a'-'z''A'-'Z''_']
let number = ['0'-'9']

(* stricter: WORDCHR = !utfrune("!\"#$%&'()*+,-./:;<=>?@[\\]^`{|}~", (r) *)
let ident = letter (letter | number)*

(*****************************************************************************)
(* Main rule *)
(*****************************************************************************)
rule token = parse

  (* ----------------------------------------------------------------------- *)
  (* Spacing/comments *)
  (* ----------------------------------------------------------------------- *)
  (* in mk, spaces have a meaning *)
  | space+        { TSpace (Lexing.lexeme lexbuf) }

  (* in mk, newline has a meaning *)
  | '\n' { incr Globals.line;
           state := if !state = AfterColon then InRecipe else Start;
           TNewline }

  (* escaped newline *)
  | '\\' '\n'     { incr Globals.line; TSpace (Lexing.lexeme lexbuf) }

  (* comments *)
  | '#' [^ '\n']* { token lexbuf }

  (* escaped newline in comment (useful to handle) *)
  | '#' [^ '\n']* '\\' '\n' 
      { incr Globals.line; 
        token lexbuf 
      }
      


  (* ----------------------------------------------------------------------- *)
  (* Symbols *)
  (* ----------------------------------------------------------------------- *)

  | ':' { state := AfterColon; TColon (loc()) }
  | '=' { if !state = AfterEq
          (* todo? means we have to normalize a series of word elements *)
          then TOther "=" 
          else begin
            state := AfterEq;
            TEq (loc()) 
          end
       }
  | '<'  { TInf (loc()) }
  | "<|" { TInfPipe (loc()) }
  | '%'  { TPercent }

  (* ----------------------------------------------------------------------- *)
  (* Variables *)
  (* ----------------------------------------------------------------------- *)

  (* stricter: force leading letter, so $0 is wrong (found bug in plan9/) *)
  | '$'    (ident as s)     { TVar s }
  | '$''{' (ident as s) '}' { TVar s }
  (* important to eat ':' otherwise would trigger a AfterColon we don't want*)
  | '$''{' (ident as s) ':' 
      { 
        (* this is to handle '=' inside ${} *)
        save_state_outside_brace := !state;
        state := InBrace;
        TVarColon s 
      }
  | '}' 
      { state := !save_state_outside_brace; 
        save_state_outside_brace := Start;
        TCBrace 
      }

  | '$' { error "missing variable name" }

  (* ----------------------------------------------------------------------- *)
  (* Quoted strings *)
  (* ----------------------------------------------------------------------- *)
  | "'" { TQuoted (quote lexbuf) }

  (* stricter: does not allow leading space *)
  (* sh syntax *)
  | "`" { TBackquoted (backquote lexbuf) }
  (* rc syntax *)
  | "`{" { TBackquoted (backquote2 lexbuf) }

  (* ----------------------------------------------------------------------- *)
  (* Regular stuff *)
  (* ----------------------------------------------------------------------- *)
  (* should be the union of the special characters mentioned before *)
  | [^'\'' '`'  '$' '{' '}'  ':' '=' '<'  '%'   '\n' '\\' '#' ' ' '\t']+ 
      { TOther (Lexing.lexeme lexbuf) }

  (* todo? means we have to normalize a series of word elements *)
  | '\\' { TOther "\\" }

  (* ----------------------------------------------------------------------- *)
  | eof { EOF }
  | _ as c   { error (spf "unrecognized character: '%c'" c) }

(*****************************************************************************)
(* Rule quote *)
(*****************************************************************************)
(* opti? could use Buffer *)
and quote = parse
  | "'"                  { "" }
  (* escaped quote by writing a double quote *)
  | "''"                 { "'" ^ quote lexbuf }

  | '\\' '\n'            { incr Globals.line; " " ^ quote lexbuf }
  (* new: better error message instead of "missing closing '"  *)
  | '\n' { error "newline in quoted string" }

  | [^ '\\' '\'' '\n']+  { let x = Lexing.lexeme lexbuf in x ^ quote lexbuf }
  | '\\' { "\\" ^ quote lexbuf }

  (* new: instead of "missing closing '"  *)
  | eof  { error "end of file in quoted string" }
  | _    { error "missing closing '" }

(*****************************************************************************)
(* Rule backquote *)
(*****************************************************************************)
(* sh syntax *)
and backquote = parse
  | "`"                  { "" }

  | '\\' '\n'            { incr Globals.line; " " ^ backquote lexbuf }
  (* new: instead of "missing closing '"  *)
  | '\n' { error "newline in backquoted string" }

  | [^ '\\' '\'' '`' '\n']+ 
      { let x = Lexing.lexeme lexbuf in x ^ backquote lexbuf }
  (* bugfix: we want to preserve the quote here! *)
  | "'" { let s = quote lexbuf in "'" ^ s ^ "'" ^ backquote lexbuf }

  (* new: instead of "missing closing `"  *)
  | eof  { error "end of file in backquoted string" }
  | _    { error "missing closing `" }

(* rc syntax *)
and backquote2 = parse
  | "}"                  { "" }

  | '\\' '\n'            { incr Globals.line; " " ^ backquote2 lexbuf }
  (* new: instead of "missing closing '"  *)
  | '\n' { error "newline in backquoted string" }

  | [^ '\\' '\'' '}' '\n']+ 
      { let x = Lexing.lexeme lexbuf in x ^ backquote2 lexbuf }
  (* bugfix: we want to preserve the quote here! *)
  | "'" { let s = quote lexbuf in "'" ^ s ^ "'" ^ backquote2 lexbuf }

  (* new: instead of "missing closing `"  *)
  | eof  { error "end of file in backquoted string" }
  | _    { error "missing closing }" }

(*****************************************************************************)
(* Rule recipe *)
(*****************************************************************************)
and recipe = parse
  | ('#'   [^'\n']*) as s '\n'? { incr Globals.line; TLineRecipe s }
  | space ([^'\n']* as s) '\n'? { incr Globals.line; TLineRecipe s }

  | [^ '#' ' ' '\t']    { state := Start; yyback 1 lexbuf; TEndRecipe }
  | eof                 { state := Start; yyback 1 lexbuf; TEndRecipe }
  | _ { error "unrecognized character in recipe" }
