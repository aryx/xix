(* Copyright 2016 Yoann Padioleau, see copyright.txt *)
open Common

let parse_line chan =

  let lexbuf = Lexing.from_channel chan in
  let lexfunc lexbuf =
    (* todo: call pprompt if doprompt *)

    (* todo: have different state after having parsed a $?
     *)
    let tok = Lexer.token lexbuf in
    (* todo: 
       - handle lastdol 
       - call pprompt() if TNewline or TBackslashNewline
       - handle SUB
       - handle free caret insertion
    *)
    tok 
    |> (fun tok -> if !Flags.dump_tokens then pr2_gen tok; tok)
  in

    try 
      Parser.rc lexfunc lexbuf
    with Parsing.Parse_error ->
      raise Parsing.Parse_error


