(* Copyright 2015, 2016 Yoann Padioleau, see copyright.txt *)
open Common

module L = Location_cpp
(* for fields access for ocaml-light *)
open Parse_cpp
open Chan

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

let parse (caps : < Cap.open_in; .. >) (conf : Preprocessor.conf) (file : Fpath.t) : Ast_asm5.program = 
  let hooks = { Parse_cpp.
     lexer = Lexer_asm5.token;
     parser = Parser_asm5.program;
     category = (fun t ->
       match t with
       | Parser_asm5.EOF -> Parse_cpp.Eof
       | Parser_asm5.TSharp -> Parse_cpp.Sharp
       | Parser_asm5.TIDENT s -> Parse_cpp.Ident s
        (* stricter: I forbid to have macros overwrite keywords *)
       | _ -> Parse_cpp.Other
     );
     eof = Parser_asm5.EOF;
  }
  in
  Parse_cpp.parse caps hooks conf file


(* Simpler code path; possibly useful in tests *)
let parse_no_cpp (chan : Chan.i) : Ast_asm5.program =
  L.line := 1;
  let lexbuf = Lexing.from_channel chan.ic in
  try 
    Parser_asm5.program Lexer_asm5.token lexbuf
  with Parsing.Parse_error ->
      failwith (spf "Syntax error: line %d" !L.line)

