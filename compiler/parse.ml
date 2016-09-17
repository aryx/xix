open Common

module T = Parser
module P = Preprocessor

let parse (defs, paths) file = 

  Globals.line := 1;

  let chan = open_in file in
  let lexbuf = Lexing.from_channel chan in

  let stack = ref [(file, chan, lexbuf)] in

  let rec lexfunc lexbuf =
    match !stack with
    | (file, chan, lexbuf)::xs ->
        let t = Lexer.token lexbuf in
        (match t with
        | T.EOF -> 
            stack := xs;
            close_in chan;
            (* todo: linehist 0 0 *)
            lexfunc lexbuf

        (* less: 
           if List.length stack > 1000
           then error "macro/io expansion too deep"
        *)
        | T.TSharp ->
            let t = Lexer_cpp.token lexbuf in
            (match t with
            | _ -> raise Todo
            );
            lexfunc lexbuf
        | _ -> t
        )
           
    | [] -> EOF
  in

  (try 
    Parser.prog lexfunc lexbuf
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

