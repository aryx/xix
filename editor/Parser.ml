(*s: Parser.ml *)
(* Copyright 2025, 2026 Yoann Padioleau, see copyright.txt *)
open Common

module T = Token

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Parsing user input using the lexer.
 *
 * alt: yacc, but overkill; peek/consume tokens seems simpler for ed use case.
 *)

(*****************************************************************************)
(* Hack for deriving show *)
(*****************************************************************************)
module Lexing_ = struct
    type lexbuf = Lexing.lexbuf
    let pp_lexbuf fmt _lexbuf =
      Format.fprintf fmt "<lexbuf>"
end

(*****************************************************************************)
(* Types *)
(*****************************************************************************)
(*s: type [[Parser.state]] *)
type state = {
  stdin: Lexing_.lexbuf;
  mutable lookahead : Token.t option;
  (*s: [[Parser.state]] other fields *)
  (* for inserting "virtual" commands to process before stdin *)
  mutable globp: Lexing_.lexbuf option;
  (*e: [[Parser.state]] other fields *)
}
(*e: type [[Parser.state]] *)
[@@deriving show]

(*s: function [[Parser.init]] *)
let init (caps : < Cap.stdin; ..>) : state =
  { stdin = Lexing.from_channel (Console.stdin caps);
    lookahead = None;
    globp = None;
  }
(*e: function [[Parser.init]] *)

(*****************************************************************************)
(* Error management *)
(*****************************************************************************)
(*s: function [[Parser.was_expecting]] *)
let was_expecting (expect : string) =
  Error.e_err (spf "was expecting %s" expect)
(*e: function [[Parser.was_expecting]] *)
(*s: function [[Parser.was_expecting_but_got]] *)
let was_expecting_but_got (expect : string) (tok : Token.t) =
  was_expecting (spf "%s, but got %s" expect (Token.show tok))
(*e: function [[Parser.was_expecting_but_got]] *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
(*s: function [[Parser.next_token]] *)
(* Do not use! this is internal! You should use peek() or consume() instead. *)
let next_token (st : state) : Token.t =
  let t = 
    match st.globp with
    (*s: [[Parser.next_token()]] match [[globp]] cases *)
    | Some lexbuf ->
        let t = Lexer.token lexbuf in
        if t = T.EOF
        then st.globp <- None;
        t
    (*e: [[Parser.next_token()]] match [[globp]] cases *)
    | None -> Lexer.token st.stdin
  in
  (*s: [[Parser.next_token()]] debug token *)
  Logs.debug (fun m -> m "tok = %s" (Token.show t));
  (*e: [[Parser.next_token()]] debug token *)
  t
(*e: function [[Parser.next_token]] *)

(*****************************************************************************)
(* peek/consume *)
(*****************************************************************************)
(*s: function [[Parser.peek]] *)
let peek (st : state) : Token.t =
  match st.lookahead with
  | Some t -> t
  | None ->
      let t = next_token st in
      st.lookahead <- Some t;
      t
(*e: function [[Parser.peek]] *)
(*s: function [[Parser.consume]] *)
let consume (st : state) : Token.t =
  match st.lookahead with
  | Some t -> st.lookahead <- None; t
  | None -> next_token st
(*e: function [[Parser.consume]] *)

(*****************************************************************************)
(* Parsing addresses *)
(*****************************************************************************)
(* Done in Address.ml *)

(*****************************************************************************)
(* Parsing Commands *)
(*****************************************************************************)
(* Done in CLI.ml instead *)

(*****************************************************************************)
(* Parsing Filenames and user text *)
(*****************************************************************************)
(* Done in In.ml instead *)
(*e: Parser.ml *)
