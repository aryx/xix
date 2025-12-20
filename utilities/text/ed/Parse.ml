
let next_token (e : Env.t) : Token.t =
  let t = Lexer.token e.stdin in
  Logs.debug (fun m -> m "tok = %s" (Token.show t));
  t

let filename (_e : Env.t) : Fpath.t =
  failwith "TODO: filename"