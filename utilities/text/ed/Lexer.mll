{
open Common
open Token

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Splitting the user input in tokens which are then assembled in
 * commands by In.ml
 *)

}
(*****************************************************************************)
(* Regexps aliases *)
(*****************************************************************************)
let space = [' ''\t']
let letter = ['a'-'z''A'-'Z''_']
let digit = ['0'-'9']

(*****************************************************************************)
(* Main rule *)
(*****************************************************************************)
rule token = parse
  | space+        { Spaces }
  | '\n'          { Newline }
  | letter as c   { Letter c }
  | digit+        { Int (int_of_string (Lexing.lexeme lexbuf)) }
  | [^' ''\n']+       { String (Lexing.lexeme lexbuf) }
  | ','  { Comma }
  | eof { EOF }

(*****************************************************************************)
(* Other rules *)
(*****************************************************************************)
and line = parse
  | [^ '\n' ]* '\n' { Lexing.lexeme lexbuf }
  | eof { failwith "eof" (* alt: None? *) }

and filename = parse
  | [^ '\n' ' ']* { Lexing.lexeme lexbuf }
  | eof { failwith "eof" }
