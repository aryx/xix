open Common
module T = Token

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Reading mostly commands from stdin *)

(*****************************************************************************)
(* Error management *)
(*****************************************************************************)

let was_expecting (expect : string) =
  Logs.err (fun m -> m "was expecting %s" expect);
  Error.e ""

let was_expecting_but_got (expect : string) (tok : Token.t) =
  was_expecting (spf "%s, but got %s" expect (Token.show tok))

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(*****************************************************************************)
(* API *)
(*****************************************************************************)

let token (e : Env.t) : Token.t =
  let t = Lexer.token e.stdin in
  Logs.debug (fun m -> m "tok = %s" (Token.show t));
  t

let newline (e : Env.t) : unit =
  match token e with
  | T.Newline | T.EOF -> ()
  (* TODO: if special chars pln ? *)
  | t -> was_expecting_but_got "newline" t

let filename (e : Env.t) (cmd : char) : Fpath.t =
  (* alt: do it in the caller, clearer; will be incremented
   * when reading the file in getfile
   *)
  e.count <- 0;

  match token e with
  | T.Newline | T.EOF ->
      (* no file specified, use maybe e.savedfile then *)
      (match e.savedfile with
      | None when cmd <> 'f' -> 
            Logs.err (fun m -> m "no savedfile and no filename given");
            Error.e ""
      | None -> failwith "TODO?? what does ed in that case?"
      | Some file -> file
      )
  | T.Spaces ->
      let str = Lexer.filename e.stdin in
      if str = ""
      then was_expecting "a non empty filename";
      (match token e with
      | T.Newline -> 
            let file = Fpath.v str in
            if e.savedfile = None || cmd = 'e' || cmd = 'f'
            then e.savedfile <- Some file;
            file
      | t -> was_expecting_but_got "a newline" t
      )
  | t -> was_expecting_but_got "a newline or space and filename" t

let gety (e : Env.t) : string =
  Lexer.line e.stdin
  

(* Read a line from stdin. Return None when the user entered ".\n" on a single
 * line meaning the end of interactive input.
 * This has a similar interface to getfile() so it can be passed to
 * append().
 *)
let gettty (e : Env.t) : string option =
  let s = gety e in
  if s = ".\n"
  then None
  else Some s
