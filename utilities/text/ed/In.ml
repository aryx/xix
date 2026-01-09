(*s: In.ml *)
(* Copyright 2025 Yoann Padioleau, see copyright.txt *)
open Common
module T = Token

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Additional helpers to read from stdin.
 *
 * alt: could be merged with Parser.ml
 *)

(*****************************************************************************)
(* API *)
(*****************************************************************************)
(*s: function [[In.newline]] *)
let newline (e : Env.t) : unit =
  match Parser.consume e.in_ with
  | T.Newline -> ()
  (* tricky but is useful to treat EOF as a newline sometimes
   * like for globp "r" but sometimes not in g/re/x globp context so that
   * it is consumed by commands() leading to returning from commands().
   *)
  | T.EOF -> ()
  (* TODO: if special chars pln ? *)
  | t -> Parser.was_expecting_but_got "newline" t
(*e: function [[In.newline]] *)

(*s: function [[In.filename]] *)
let filename (e : Env.t) (cmd : char) : Fpath.t =
  (* alt: do it in the caller, clearer; will be incremented
   * when reading the file in getfile
   *)
  e.count <- 0;

  match Parser.consume e.in_ with
  | T.Newline | T.EOF ->
      (* no file specified, use maybe e.savedfile then *)
      (match e.savedfile with
      | None when cmd <> 'f' -> Error.e_err "no savedfile and no filename given"
      | None -> failwith "TODO?? what does ed in that case?"
      | Some file -> file
      )
  | T.Spaces ->
      let str = Lexer.filename e.in_.stdin in
      if str = ""
      then Parser.was_expecting "a non empty filename";
      (match Parser.consume e.in_ with
      | T.Newline -> 
            let file = Fpath.v str in
            if e.savedfile = None || cmd = 'e' || cmd = 'f'
            then e.savedfile <- Some file;
            file
      | t -> Parser.was_expecting_but_got "a newline" t
      )
  | t -> Parser.was_expecting_but_got "a newline or space and filename" t
(*e: function [[In.filename]] *)

(*s: function [[In.gety]] *)
(* return a line (without trailing '\n') *)
let gety (e : Env.t) : string =
  Lexer.line e.in_.stdin
(*e: function [[In.gety]] *)
(*s: function [[In.gettty]] *)
(* Read a line from stdin. Return None when the user entered "." on a single
 * line meaning the end of interactive input.
 * This has a similar interface to getfile() so it can be passed to
 * append().
 *)
let gettty (e : Env.t) () : string option =
  let s = gety e in
  if s = "."
  then begin
    Logs.info (fun m -> m "end of input, back to ed");
    None
  end
  else Some s
(*e: function [[In.gettty]] *)
(*e: In.ml *)
