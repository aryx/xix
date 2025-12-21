open Common
module T = Token

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Reading mostly commands from stdin *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let unexpected (t : Token.t) =
  Logs.err (fun m -> m "unexpected token: %s" (Token.show t));
  Error.e ""

(*****************************************************************************)
(* API *)
(*****************************************************************************)

let token (e : Env.t) : Token.t =
  let t = Lexer.token e.stdin in
  Logs.debug (fun m -> m "tok = %s" (Token.show t));
  t

let filename (e : Env.t) (cmd : char) : Fpath.t =
  (* alt: do it in the caller, clearer; will be incremented
   * when reading the file in getfile
   *)
  e.count <- 0;
  match token e with
  | T.Newline | T.EOF ->
      (* no file specified, use maybe e.savedfile then *)
      (match e.savedfile with
      | None when cmd <> 'f' -> Error.e ""
      | None -> failwith "TODO?? what does ed in that case?"
      | Some file -> file
      )
  | T.Spaces ->
      (match token e with
      (* TODO? in theory could also be Letter c or Int for weird filenames 
       * TODO: maybe could call another Lexer.filename func!
       *)
      | T.String str ->
          (match token e with
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


(* Read a line from stdin. Return None when the user entered ".\n" on a single
 * line meaning the end of interactive input.
 *)
let gettty (_e : Env.t) : string option =
  failwith "TODO: getty"
