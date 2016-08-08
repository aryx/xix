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
      | Lexer.InRecipe -> 
          Lexer.recipe lexbuf
      ) |> (fun tok -> if !Flags.dump_tokens then pr2_gen tok; tok)
    in
      
    try 
      Parser.program lexfunc lexbuf
    (* less: could track line of : and = *)
    with Parsing.Parse_error ->
      failwith (spf "%s:%d: Syntax error" !Globals.file !Globals.line)
  )
