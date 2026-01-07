(*s: Address.ml *)
(* Copyright 2025, 2026 Yoann Padioleau, see copyright.txt *)
open Common

module P = Parser
module T = Token

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(*s: type [[Address.t]] *)
(* An "address" is a way to specify a line number symbolically or literally *)
type t =
  | Current (* '.' *)
  | Last    (* '$' *)
  | Line of int (* <n> *)
  | Mark of char (* \a *)
  | SearchFwd of string (* /.../ *)
  | SearchBwd of string (* ?...? *)
  | Relative of t * int (* -, +, ^ *)
(*e: type [[Address.t]] *)
[@@deriving show]
(*s: type [[Address.range]] *)
(* What is parsed before a command. For instance 1,3p will be parsed as
 * { addr1 = Some (Line 1); addr2 = Line 3; given = true; set_dot = false}.
 *)
type range = {
  addr1 : t option;
  addr2 : t;
  given : bool;
  set_dot : bool;
}
(*e: type [[Address.range]] *)
[@@deriving show]
(*e: Address.ml *)

(*****************************************************************************)
(* Parsing *)
(*****************************************************************************)

(*
val parse_address_range: state -> Address.range
*)

let parse_delta (st : Parser.state) : int =
  match P.consume st with
  | T.Plus ->
      (match P.peek st with
       | T.Int n -> ignore (P.consume st); n
       | _ -> 1)
  | T.Minus ->
      (match P.peek st with
       | T.Int n -> ignore (P.consume st); -n
       | _ -> -1)
  | T.Caret -> -1
  | _ ->
      P.was_expecting "relative operator"

let rec parse_relatives (base : t) (st : Parser.state) : t =
  match P.peek st with
  | T.Plus | T.Minus | T.Caret ->
      let d = parse_delta st in
      parse_relatives (Relative (base, d)) st
  | _ ->
      base

let parse_address (st : Parser.state) : t =
  let base =
    match P.peek st with
    | T.Plus | T.Minus | T.Caret ->
      (* implicit '.' for leading + - ^ *)
      Current
    | _ ->
        (match P.consume st with
        | T.Dot -> Current
        | T.Dollar -> Last
        | T.Int n -> Line n
        | T.Mark c -> Mark c
        | T.Slash r -> SearchFwd r
        | T.Question r -> SearchBwd r
        | _ -> P.was_expecting "valid address"
        )
  in
  parse_relatives base st

let parse_address_range (st : Parser.state) : range =
  let t1 = P.peek st in
  (* optional first address *)
  let first : t option =
    match t1 with
    | T.Plus | T.Minus | T.Caret
    | T.Dot | T.Dollar | T.Int _ | T.Mark _ | T.Slash _ | T.Question _
      ->
        (* this will consume some tokens in st *)
        Some (parse_address st)
    | T.Comma | T.Semicolon ->
        None
    | T.EOF | T.Spaces | T.Newline | T.Char _ ->
        None
  in

  let t2 = P.peek st in
  match t2 with
  | T.Comma | T.Semicolon ->
      P.consume st |> ignore;
      let second = 
        match P.peek st with
        | T.Plus | T.Minus | T.Caret
          | T.Dot | T.Dollar | T.Int _ | T.Mark _ | T.Slash _ | T.Question _
          -> parse_address st 
        (* a missing second address default to '$' *)
        | T.EOF | T.Spaces | T.Newline | T.Char _ | T.Comma | T.Semicolon 
          -> Last
      in
      {
         (* a missing first address in a range default to '1' so
          * a range like "," will be parsed as "1,$"
          *)
          addr1 = (match first with Some _ -> first | None -> Some (Line 1)); 
          addr2 = second;
          given = true; 
          set_dot = (t2 = T.Semicolon);
         }
  | _ ->
      (* single address or none *)
      (match first with
      | Some a ->
          { addr1 = None; addr2 = a; given = true; set_dot = false; }
      | None ->
          { addr1 = None; addr2 = Current; given = false; set_dot = false; }
      )

(*****************************************************************************)
(* Evaluating *)
(*****************************************************************************)
