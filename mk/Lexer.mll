(*s: Lexer.mll *)
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
(*s: function [[Lexer.error]] *)
let error s =
  failwith (spf "%s:%d: Lexical error, %s" !Globals.file !Globals.line s)
(*e: function [[Lexer.error]] *)
(*s: function [[Lexer.loc]] *)
let loc () = 
  { Ast.file = Fpath.v !Globals.file; Ast.line = !Globals.line; }
(*e: function [[Lexer.loc]] *)

(*s: type [[Lexer.state]] *)
(* lexer state *)
type state = 
  | Start
  (* once we started to parse a rule, the next newline will start a recipe *)
  | AfterColon
  (* the lexing rules are different in a recipe; we do not parse rc's input *)
  | InRecipe
  (*s: [[Lexer.state]] other cases *)
  (* once we started to parse an assign, the second = is like a string *)
  | AfterEq
  (*x: [[Lexer.state]] other cases *)
  (* except inside ${x:...=...} where we still want = to be TEq *)
  | InBrace
  (*e: [[Lexer.state]] other cases *)
(*e: type [[Lexer.state]] *)
(*s: global [[Lexer.state_]] *)
(* see also parse.ml and code using that global
 * ocaml-light: renamed to state_ cos conflict with state var used by ocamllex
 *)
let state_ = ref Start
(*e: global [[Lexer.state_]] *)

(*s: Lexer helpers *)
(*s: global [[Lexer.save_state__outside_brace]] *)
(* A single var is enough since mk does not allow recursivity in braces
 * as in ${x:%${y}x=%.c}. We do not need a stack.
 * *)
let save_state_outside_brace = ref Start
(*e: global [[Lexer.save_state__outside_brace]] *)
(*s: function [[Lexer.yyback]] *)
(* pad: hack around ocamllex to emulate the yyless() of flex. The semantic
 * is not exactly the same than yyless(), so I use yyback() instead.
 * http://my.safaribooksonline.com/book/programming/flex/9780596805418/a-reference-for-flex-specifications/yyless
 *)
let yyback n lexbuf =
  lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_curr_pos - n;
  ()
  (* ocaml-light: lex_curr_p does not exist in ocaml-light, but
     TODO? anyway do we need this code?
  let currp = lexbuf.Lexing.lex_curr_p in
  lexbuf.Lexing.lex_curr_p <- { currp with
    Lexing.pos_cnum = currp.Lexing.pos_cnum - n;
  }
  *)
(*e: function [[Lexer.yyback]] *)
(*e: Lexer helpers *)
}
(*****************************************************************************)
(* Regexps aliases *)
(*****************************************************************************)
(*s: lexer regexp aliases *)
let space = [' ''\t']
(*x: lexer regexp aliases *)
let letter = ['a'-'z''A'-'Z''_']
let number = ['0'-'9']

(* stricter: WORDCHR = !utfrune("!\"#$%&'()*+,-./:;<=>?@[\\]^`{|}~", (r) *)
let ident = letter (letter | number)*
(*e: lexer regexp aliases *)

(*****************************************************************************)
(* Main rule *)
(*****************************************************************************)
(*s: rule [[Lexer.token]] *)
rule token = parse

  (* ----------------------------------------------------------------------- *)
  (* Spacing/comments *)
  (* ----------------------------------------------------------------------- *)
  (*s: [[Lexer.token()]] space cases *)
  (* in mk, spaces have a meaning *)
  | space+        { TSpace (Lexing.lexeme lexbuf) }
  (*x: [[Lexer.token()]] space cases *)
  (* in mk, newline has a meaning *)
  | '\n' { incr Globals.line;
           state_ := if !state_ = AfterColon then InRecipe else Start;
           TNewline }
  (*x: [[Lexer.token()]] space cases *)
  (* escaped newline *)
  | '\\' '\n'     { incr Globals.line; TSpace (Lexing.lexeme lexbuf) }
  (*e: [[Lexer.token()]] space cases *)
  (*s: [[Lexer.token()]] comment cases *)
  (* comments *)
  | '#' [^ '\n']* { token lexbuf }

  (* escaped newline in comment (useful to handle) *)
  | '#' [^ '\n']* '\\' '\n' 
      { incr Globals.line; 
        token lexbuf 
      }
  (*e: [[Lexer.token()]] comment cases *)
      
  (* ----------------------------------------------------------------------- *)
  (* Symbols *)
  (* ----------------------------------------------------------------------- *)
  (*s: [[Lexer.token()]] symbol cases *)
  | ':' { state_ := AfterColon; TColon (loc()) }
  (*x: [[Lexer.token()]] symbol cases *)
  | '<'  { TInf (loc()) }
  (*x: [[Lexer.token()]] symbol cases *)
  | '%'  { TPercent }
  (*x: [[Lexer.token()]] symbol cases *)
  | '=' { if !state_ <> AfterEq
          (* todo? means we have to normalize a series of word elements *)
          then begin
            state_ := AfterEq;
            TEq (loc()) 
          end
          else TOther "=" 
       }
  (*x: [[Lexer.token()]] symbol cases *)
  | "<|" { TInfPipe (loc()) }
  (*e: [[Lexer.token()]] symbol cases *)

  (* ----------------------------------------------------------------------- *)
  (* Variables *)
  (* ----------------------------------------------------------------------- *)
  (*s: [[Lexer.token()]] variable cases *)
  (* stricter: force leading letter, so $0 is wrong (found bug in plan9/) *)
  | '$'    (ident (*as s*))
      { let s = Lexing.lexeme lexbuf in TVar (String.sub s 1 (String.length s - 1)) }
  (*x: [[Lexer.token()]] variable cases *)
  | '$''{' (ident (*as s*)) '}'
      { let s = Lexing.lexeme lexbuf in TVar (String.sub s 2 (String.length s - 3)) }
  (*x: [[Lexer.token()]] variable cases *)
  (* important to eat ':' otherwise would trigger a AfterColon we don't want *)
  | '$' '{' (ident (*as s*)) ':'
      {
        let s = Lexing.lexeme lexbuf in
        (* this is to handle '=' inside ${} *)
        save_state_outside_brace := !state_;
        state_ := InBrace;
        TVarColon (String.sub s 2 (String.length s - 3)) 
      }
  | '}' 
      { state_ := !save_state_outside_brace; 
        save_state_outside_brace := Start;
        TCBrace 
      }

  | '$' { error "missing variable name" }
  (*e: [[Lexer.token()]] variable cases *)

  (* ----------------------------------------------------------------------- *)
  (* Quoted strings *)
  (* ----------------------------------------------------------------------- *)
  (*s: [[Lexer.token()]] quoted string cases *)
  | "'" { TQuoted (quote lexbuf) }
  (*x: [[Lexer.token()]] quoted string cases *)
  (* stricter: does not allow leading space *)
  (* sh syntax *)
  | "`" { TBackquoted (backquote lexbuf) }
  (* rc syntax *)
  | "`{" { TBackquoted (backquote2 lexbuf) }
  (*e: [[Lexer.token()]] quoted string cases *)

  (* ----------------------------------------------------------------------- *)
  (* Regular stuff *)
  (* ----------------------------------------------------------------------- *)
  (*s: [[Lexer.token()]] other cases *)
  (* should be the union of the special characters mentioned before *)
  | [^'\'' '`'  '$' '{' '}'  ':' '=' '<'  '%'   '\n' '\\' '#' ' ' '\t']+ 
      { TOther (Lexing.lexeme lexbuf) }
  (*x: [[Lexer.token()]] other cases *)
  (* todo? means we have to normalize a series of word elements *)
  | '\\' { TOther "\\" }
  (*e: [[Lexer.token()]] other cases *)

  (* ----------------------------------------------------------------------- *)
  | eof { EOF }
  | _ (*as c*)   { error (spf "unrecognized character: '%s'" (Lexing.lexeme lexbuf)) }
(*e: rule [[Lexer.token]] *)

(*****************************************************************************)
(* Rule quote *)
(*****************************************************************************)
(*s: rule [[Lexer.quote]] *)
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
(*e: rule [[Lexer.quote]] *)

(*****************************************************************************)
(* Rule recipe *)
(*****************************************************************************)
(*s: rule [[Lexer.recipe]] *)
and recipe = parse
  | ('#'   [^'\n']*) (*as s*) '\n'?
      { let s = Lexing.lexeme lexbuf |> String.trim in
        incr Globals.line; TLineRecipe s }
  | space ([^'\n']* (*as s*)) '\n'?
      { let s = Lexing.lexeme lexbuf |> String.trim in
         incr Globals.line; TLineRecipe s }
  | [^ '#' ' ' '\t']    { state_ := Start; yyback 1 lexbuf; TEndRecipe }
  | eof                 { state_ := Start; yyback 1 lexbuf; TEndRecipe }
  | _ {error "unrecognized character in recipe" }
(*e: rule [[Lexer.recipe]] *)

(*****************************************************************************)
(* Other Rules *)
(*****************************************************************************)
(*s: Lexer other rules *)
(*s: rule [[Lexer.backquote]] *)
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
(*e: rule [[Lexer.backquote]] *)
(*s: rule [[Lexer.backquote2]] *)
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
  (* TODO: if use } below get weird ocamllex error *)
  |_ { error "missing closing brace" }
(*e: rule [[Lexer.backquote2]] *)
(*e: Lexer other rules *)
(*e: Lexer.mll *)
