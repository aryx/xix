(*s: mk/Parse.ml *)
(* Copyright 2016 Yoann Padioleau, see copyright.txt *)
open Common
open Fpath_.Operators

(*s: function [[Parse.parse]] *)
let parse (file : Fpath.t) : Ast.instr list =
  file |> UChan.with_open_in (fun (chan : Chan.i) ->
    Globals.line := 1;
    Globals.file := !!file;

    let lexbuf = Lexing.from_channel chan.Chan.ic in
    (*s: [[Parse.parse()]] nested [[lexfunc]] function *)
    Lexer.state_ := Lexer.Start;
    let lexfunc lexbuf =
      let tok =
        match !Lexer.state_ with
        | Lexer.Start 
        | Lexer.AfterColon 
        | Lexer.AfterEq 
        | Lexer.InBrace -> 
            Lexer.token lexbuf
        | Lexer.InRecipe -> 
            Lexer.recipe lexbuf
      in
      (*s: [[Parse.parse()]] [[lexfunc()]] possibly dump the token *)
      if !Flags.dump_tokens 
      then Logs.app (fun m -> m "%s" (Dumper.dump tok));
      (*e: [[Parse.parse()]] [[lexfunc()]] possibly dump the token *)
      tok
    in
    (*e: [[Parse.parse()]] nested [[lexfunc]] function *)
    try 
      Parser.program lexfunc lexbuf
    (* less: could track line of : and = *)
    with Parsing.Parse_error ->
      failwith (spf "%s:%d: Syntax error" !Globals.file !Globals.line)
  )
(*e: function [[Parse.parse]] *)
(*e: mk/Parse.ml *)
