(* Copyright 2015, 2016 Yoann Padioleau, see copyright.txt *)

let parse file =
  file |> Common.with_file_in (fun chan ->
    Globals.line := 1;
    let lexbuf = Lexing.from_channel chan in
    Parser_asm5.program Lexer_asm5.token lexbuf
  )

