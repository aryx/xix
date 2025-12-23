(* Copyright 2025 Yoann Padioleau, see copyright.txt *)
open Common
module A = Address
module T = Token

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Parsing user input using the lexer.
 *
 * alt: yacc, but overkill; recursive descent seems simpler for ed use case.
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type state = {
  stdin: Lexing_.lexbuf;
  (* for insert "virtual" commands to process before stdin *)
  mutable globp: Lexing_.lexbuf option;
  mutable lookahead : Token.t option;
}
[@@deriving show]

let init (caps : < Cap.stdin; ..>) : state =
  { stdin = Lexing.from_channel (Console.stdin caps);
    globp = None;
    lookahead = None;
  }

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

(* Do not use! this is internal! You should use peek() or consume() instead. *)
let next_token (st : state) : Token.t =
  let t = 
    match st.globp with
    | Some lexbuf ->
        let t = Lexer.token lexbuf in
        if t = T.EOF
        then st.globp <- None;
        t
    | None ->
        Lexer.token st.stdin
  in
  Logs.debug (fun m -> m "tok = %s" (Token.show t));
  t

(*****************************************************************************)
(* peek/consume *)
(*****************************************************************************)
         
let peek (st : state) : Token.t =
  match st.lookahead with
  | Some t -> t
  | None ->
      let t = next_token st in
      st.lookahead <- Some t;
      t

let consume (st : state) : Token.t =
  match st.lookahead with
  | Some t -> st.lookahead <- None; t
  | None -> next_token st

(*****************************************************************************)
(* Parsing addresses *)
(*****************************************************************************)

let parse_delta (st : state) : int =
  match consume st with
  | T.Plus ->
      (match peek st with
       | T.Int n -> ignore (consume st); n
       | _ -> 1)
  | T.Minus ->
      (match peek st with
       | T.Int n -> ignore (consume st); -n
       | _ -> -1)
  | T.Caret -> -1
  | _ ->
      was_expecting "relative operator"

let rec parse_relatives (base : A.t) (st : state) : A.t =
  match peek st with
  | T.Plus | T.Minus | T.Caret ->
      let d = parse_delta st in
      parse_relatives (A.Relative (base, d)) st
  | _ ->
      base

let parse_address (st : state) : A.t =
  match peek st with
  (* implicit '.' for leading + - ^ *)
  | T.Plus | T.Minus | T.Caret ->
      parse_relatives A.Current st

  | _ ->
      let base =
        match consume st with
        | T.Dot -> A.Current
        | T.Dollar -> A.Last
        | T.Int n -> A.Line n
        | T.Mark c -> A.Mark c
        | T.Slash r -> A.SearchFwd r
        | T.Question r -> A.SearchBwd r
        | _ -> was_expecting "valid address"
      in
      parse_relatives base st


let parse_address_range (st : state) : A.range =
  (* optional first address *)
  let first =
    match peek st with
    | T.Plus | T.Minus | T.Caret
    | T.Dot | T.Dollar | T.Int _ | T.Mark _ | T.Slash _ | T.Question _
      ->
        Some (parse_address st)
    | T.Comma | T.Semicolon ->
        None
    | T.EOF | T.Spaces | T.Newline | T.Char _ ->
        None
  in

  match peek st with
  | T.Comma ->
      ignore (consume st);
      let second = parse_address st in
      A.{ addr1 = first; addr2 = second; given = true; set_dot = false; }
  | T.Semicolon ->
      ignore (consume st);
      let second = parse_address st in
      A.{ addr1 = first; addr2 = second; given = true; set_dot = true; }
  | _ ->
      (* single address or none *)
      (match first with
      | Some a ->
          A.{ addr1 = None; addr2 = a; given = true; set_dot = false; }
      | None ->
          A.{ addr1 = None; addr2 = Current; given = false; set_dot = false; }
      )

(*****************************************************************************)
(* Parsing Commands *)
(*****************************************************************************)
(* Done in CLI.ml instead *)

(*****************************************************************************)
(* Parsing Filenames and user text *)
(*****************************************************************************)
(* Done in In.ml instead *)
