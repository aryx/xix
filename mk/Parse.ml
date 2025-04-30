(* Copyright 2016 Yoann Padioleau, see copyright.txt *)
open Stdcompat (* for |> *)
open Common
open Fpath.Operators

let parse (file : Fpath.t) =
  file |> UChan.with_open_in (fun (chan : Chan.i) ->
    Globals.line := 1;
    Globals.file := !!file;
    Lexer.state := Lexer.Start;

    let lexbuf = Lexing.from_channel chan.ic in
    let lexfunc lexbuf =
      (match !Lexer.state with
      | Lexer.Start 
      | Lexer.AfterColon 
      | Lexer.AfterEq 
      | Lexer.InBrace -> 
          Lexer.token lexbuf
      | Lexer.InRecipe -> 
          Lexer.recipe lexbuf
      ) |> (fun tok -> 
            if !Flags.dump_tokens 
            then Logs.app (fun m -> m "%s" (Dumper.dump tok));
            tok)
    in
      
    try 
      Parser.program lexfunc lexbuf
    (* less: could track line of : and = *)
    with Parsing.Parse_error ->
      failwith (spf "%s:%d: Syntax error" !Globals.file !Globals.line)
  )
