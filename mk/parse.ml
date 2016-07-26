(* Copyright 2016 Yoann Padioleau, see copyright.txt *)
open Common

let parse file =
  file |> Common.with_file_in (fun chan ->
    Globals.line := 1;
    Globals.file := file;
    Lexer.state := Lexer.Start;

    let lexbuf = Lexing.from_channel chan in
    let lexfunc lexbuf =
      match !Lexer.state with
      | Lexer.Start | Lexer.AfterColon -> Lexer.token lexbuf
      | Lexer.InRecipe -> Lexer.recipe lexbuf
    in
      
    try 
      Parser.program lexfunc lexbuf
    with Parsing.Parse_error ->
      failwith (spf "%s:%d: Syntax error" !Globals.file !Globals.line)
  )
