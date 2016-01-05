
let parse file =
  file |> Common.with_file_in (fun chan ->
    let lexbuf = Lexing.from_channel chan in
    Parser_asm5.program Lexer_asm5.token lexbuf
  )

