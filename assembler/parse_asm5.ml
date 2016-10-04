(* Copyright 2015, 2016 Yoann Padioleau, see copyright.txt *)
open Common

module L = Location_cpp
module T = Parser_asm5  (* T for Tokens *)

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

let parse (defs, paths) file = 
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
  Parse_cpp.parse hooks (defs, paths) file


let parse_no_cpp file =
  file |> Common.with_file_in (fun chan ->
    L.line := 1;
    let lexbuf = Lexing.from_channel chan in
    try 
      Parser_asm5.program Lexer_asm5.token lexbuf
    with Parsing.Parse_error ->
      failwith (spf "Syntax error: line %d" !L.line)
  )
