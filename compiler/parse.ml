open Common


let parse (defs, paths) file = 
  (* TODO: plug call to cpp *)
  let chan = open_in file in
  Globals.line := 1;
  let lexbuf = Lexing.from_channel chan in
  (try 
    Parser.prog Lexer.token lexbuf
  with Parsing.Parse_error ->
    failwith (spf "Syntax error: line %d" !Globals.line)
  )



let parse_no_cpp file =
  file |> Common.with_file_in (fun chan ->
    Globals.line := 1;
    let lexbuf = Lexing.from_channel chan in
    (try 
      Parser.prog Lexer.token lexbuf
    with Parsing.Parse_error ->
      failwith (spf "Syntax error: line %d" !Globals.line)
    )
  )

