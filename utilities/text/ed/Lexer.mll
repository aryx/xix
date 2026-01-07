(*s: Lexer.mll *)
{
open Common
open Token

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Splitting the user input in tokens which are then assembled in
 * addresses or commands by Parser.ml
 *)

(*s: constant [[Lexer.buf]] *)
let buf = Buffer.create 32
}
(*e: constant [[Lexer.buf]] *)
(*****************************************************************************)
(* Regexps aliases *)
(*****************************************************************************)
(*s: constant [[Lexer.space]] *)
let space = [' ''\t']
(*e: constant [[Lexer.space]] *)
(*s: constant [[Lexer.letter]] *)
let letter = ['a'-'z''A'-'Z''_']
(*e: constant [[Lexer.letter]] *)
(*s: constant [[Lexer.digit]] *)
let digit = ['0'-'9']
(*e: constant [[Lexer.digit]] *)

(*****************************************************************************)
(* Main rule *)
(*****************************************************************************)
(*s: function [[Lexer.token]] *)
rule token = parse
  | space+        { Spaces }
  | '\n'          { Newline }

  (* for the command *)
  | (letter | '=' | '!') as c   { Char c }

  (* for the addresses *)
  | digit+        { Int (int_of_string (Lexing.lexeme lexbuf)) }
  | '.' { Dot } | '$' { Dollar }


  | ',' { Comma } | ';' { Semicolon }
  | '+' { Plus } | '-' { Minus } | '^' { Caret }
  | '\'' ['a'-'z'] as s { Mark s.[1] }
  | '/'              { Buffer.clear buf; Slash (regexp '/' lexbuf) }
  | '?'              { Buffer.clear buf; Question (regexp '?' lexbuf) }

  | eof { EOF }
(*e: function [[Lexer.token]] *)
(*****************************************************************************)
(* Regexp rule *)
(*****************************************************************************)
(*s: function [[Lexer.regexp]] *)
and regexp delim = parse
  | '\\' (_ as c) {
      Buffer.add_char buf '\\'; Buffer.add_char buf c;
      regexp delim lexbuf
    }
  | '/' {
      if delim = '/'
      then Buffer.contents buf
      else begin
        Buffer.add_char buf '/';
        regexp delim lexbuf
      end
     }
  | '?' {
      if delim = '?'
      then Buffer.contents buf
      else begin
        Buffer.add_char buf '?';
        regexp delim lexbuf
      end
    }
  | '\n' | eof {
      failwith "Unterminated regular expression"
    }
  | _ as c {
      Buffer.add_char buf c;
      regexp delim lexbuf
    }
(*e: function [[Lexer.regexp]] *)

(*****************************************************************************)
(* Other rules *)
(*****************************************************************************)
(*s: function [[Lexer.line]] *)
and line = parse
  | ([^ '\n' ]* as s) '\n' { s }
  | eof { failwith "eof in Lexer.line()" (* alt: None? *) }
(*e: function [[Lexer.line]] *)
(*s: function [[Lexer.filename]] *)
and filename = parse
  | [^ '\n' ' ']* { Lexing.lexeme lexbuf }
  | eof { failwith "eof in Lexer.filename()" }
(*e: function [[Lexer.filename]] *)
(*e: Lexer.mll *)
