open Common
module T = Token

let unexpected (t : Token.t) =
  failwith (spf "unexpected token: %s" (Token.show t))  

let next_token (e : Env.t) : Token.t =
  let t = Lexer.token e.stdin in
  Logs.debug (fun m -> m "tok = %s" (Token.show t));
  t

let filename (e : Env.t) (cmd : char) : Fpath.t =
  (* TODO? e.count <- 0 ? *)
  match next_token e with
  | T.Newline | T.EOF ->
      (* no file specified, use maybe e.savedfile then *)
      (match e.savedfile with
      | None when cmd <> 'f' -> Error.e ""
      | None -> failwith "TODO?? what does ed in that case?"
      | Some file -> file
      )
  | T.Spaces ->
      (match next_token e with
      (* TODO? in theory could also be Letter c or Int for weird filenames *)
      | T.String str ->
          (match next_token e with
          | T.Newline -> 
                  let file = Fpath.v str in
                  if e.savedfile = None || cmd = 'e' || cmd = 'f'
                  then e.savedfile <- Some file;
                  file
          | _ -> Error.e ""
          )
      | _ -> Error.e ""
      )
  | _ -> Error.e ""
