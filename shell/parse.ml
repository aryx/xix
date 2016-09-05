(* Copyright 2016 Yoann Padioleau, see copyright.txt *)
open Common

let parse_line chan =

  let _lexbuf = Lexing.from_channel chan in

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
  in
  raise Todo
