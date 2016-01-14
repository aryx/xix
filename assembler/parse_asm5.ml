(* Copyright 2015, 2016 Yoann Padioleau, see copyright.txt *)
open Common

let parse file =
  file |> Common.with_file_in (fun chan ->
    Globals.line := 1;
    let lexbuf = Lexing.from_channel chan in
    try 
      Parser_asm5.program Lexer_asm5.token lexbuf
    with Parsing.Parse_error ->
      failwith (spf "Syntax error: line %d" !Globals.line)
  )

