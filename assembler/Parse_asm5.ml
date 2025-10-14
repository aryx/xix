(* Copyright 2015, 2016 Yoann Padioleau, see copyright.txt *)
open Common

module L = Location_cpp
module T = Parser_asm5  (* T for Tokens *)
(* for fields access for ocaml-light *)
open Parse_cpp
open Chan

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

let parse (conf : Preprocessor.conf) (file : Fpath.t) : Ast_asm5.program = 
  let hooks = { Parse_cpp.
     lexer = Lexer_asm5.token;
     category = (fun t ->
       match t with
       | T.EOF -> Parse_cpp.Eof
       | T.TSharp -> Parse_cpp.Sharp
       | T.TIDENT s -> Parse_cpp.Ident s
        (* stricter: I forbid to have macros overwrite keywords *)
       | _ -> Parse_cpp.Other
     );
   parser = Parser_asm5.program;
   eof = T.EOF;
  }
  in
  Parse_cpp.parse hooks conf file


(* ?? what for ? *)
let parse_no_cpp (file : Fpath.t) : Ast_asm5.program =
  file |> UChan.with_open_in (fun (chan : Chan.i) ->
    L.line := 1;
    let lexbuf = Lexing.from_channel chan.ic in
    try 
      Parser_asm5.program Lexer_asm5.token lexbuf
    with Parsing.Parse_error ->
      failwith (spf "Syntax error: line %d" !L.line)
  )
