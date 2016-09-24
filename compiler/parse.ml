open Common

module T = Parser  (* T for Tokens *)
module D = Ast_cpp (* D for Directives *)
module L = Location_cpp
module P = Preprocessor

let parse (defs, paths) file = 

  L.line := 1;
  defs |> List.iter Preprocessor.define_cmdline_def;

  let chan = open_in file in
  let lexbuf = Lexing.from_channel chan in
  let stack = ref [(file, chan, lexbuf)] in

  let rec lexfunc () =
    match !stack with
    | (file, chan, lexbuf)::xs ->
        let t = Lexer.token lexbuf in
        (match t with
        | T.EOF -> 
            stack := xs;
            close_in chan;
            (* todo: linehist 0 0 *)
            lexfunc ()
        (* less: 
           if List.length stack > 1000
           then error "macro/io expansion too deep"
        *)

        | T.TSharp ->
            let t = Lexer_cpp.token lexbuf in
            (match t with
            | D.Include (f, system) ->
              let path = Preprocessor.find_include paths (f, system) in
              let chan = open_in path in
              let lexbuf = Lexing.from_channel chan in
              stack := (path, chan, lexbuf)::!stack;

            | D.Define (s, params, body) ->
                Preprocessor.define (s, params, body)

            | D.Line _
            | D.Undef _
            | D.Ifdef _
            | D.Ifndef _
            | D.Else
            | D.Endif
            | D.Pragma _
                -> raise Todo
            );
            lexfunc ()
        | _ -> t
        )
           
    | [] -> T.EOF
  in

  (try 
    Parser.prog (fun _lexbuf -> lexfunc ()) lexbuf
  with Parsing.Parse_error ->
    raise (Error.Error (spf "Syntax error", !L.line))
  )



let parse_no_cpp file =
  file |> Common.with_file_in (fun chan ->
    L.line := 1;
    let lexbuf = Lexing.from_channel chan in
    (try 
      Parser.prog Lexer.token lexbuf
    with Parsing.Parse_error ->
      (* less: record last name, and do like 5c *)
      failwith (spf "Syntax error: line %d" !L.line)
    )
  )
