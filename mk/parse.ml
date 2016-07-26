(* Copyright 2016 Yoann Padioleau, see copyright.txt *)
open Common

let parse file =
  file |> Common.with_file_in (fun chan ->
    Globals.line := 1;
    Globals.file := file;
    Lexer.state := Lexer.Start;

    let lexbuf = Lexing.from_channel chan in
    let lexfunc lexbuf =
      (match !Lexer.state with
      | Lexer.Start 
      | Lexer.AfterColon 
      | Lexer.AfterEq 
      | Lexer.InBrace -> 
          Lexer.token lexbuf
      | Lexer.InRecipe -> Lexer.recipe lexbuf
      ) |> (fun tok -> if !Flags.debug_lexer then pr2_gen tok; tok)
    in
      
    try 
      Parser.program lexfunc lexbuf
      |> (fun ast -> if !Flags.debug_ast then ast |> List.iter pr2_gen; ast)

    with Parsing.Parse_error ->
      failwith (spf "%s:%d: Syntax error" !Globals.file !Globals.line)
  )
