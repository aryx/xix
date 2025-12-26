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

(*****************************************************************************)
(* Main rule *)
(*****************************************************************************)
rule token = parse
  | space+        { Spaces }
  | '\n'          { Newline }

  (* for the command *)
  | (letter | '=') as c   { Char c }

  (* for the addresses *)
  | digit+        { Int (int_of_string (Lexing.lexeme lexbuf)) }
  | '.' { Dot } | '$' { Dollar }
(*e: constant [[Lexer.digit]] *)
  | ',' { Comma } | ';' { Semicolon }
  | '+' { Plus } | '-' { Minus } | '^' { Caret }
  | '\'' ['a'-'z'] as s { Mark s.[1] }
  | '/'              { Buffer.clear buf; regexp '/' lexbuf }
  | '?'              { Buffer.clear buf; regexp '?' lexbuf }

  | eof { EOF }

and regexp delim = parse
  | '\\' (_ as c) {
      Buffer.add_char buf '\\'; Buffer.add_char buf c;
      regexp delim lexbuf
    }
  | '/' {
      if delim = '/'
      then Slash (Buffer.contents buf)
      else begin
        Buffer.add_char buf '/';
        regexp delim lexbuf
      end
     }
  | '?' {
      if delim = '?'
      then Question (Buffer.contents buf)
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

(*****************************************************************************)
(* Other rules *)
(*****************************************************************************)
and line = parse
  | ([^ '\n' ]* as s) '\n' { s }
  | eof { failwith "eof" (* alt: None? *) }

and filename = parse
  | [^ '\n' ' ']* { Lexing.lexeme lexbuf }
  | eof { failwith "eof" }
(*e: Lexer.mll *)
