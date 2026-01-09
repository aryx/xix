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
  | Relative of t * int (* -, +, ^ *)
  (*s: [[Address.t]] other cases *)
  | SearchFwd of string (* /.../ *)
  | SearchBwd of string (* ?...? *)
  (*x: [[Address.t]] other cases *)
  | Mark of char (* \a *)
  (*e: [[Address.t]] other cases *)
(*e: type [[Address.t]] *)
[@@deriving show]
(*s: type [[Address.range]] *)
(* What is parsed before a command. For instance 1,3 will be parsed as
 * { addr1 = Some (Line 1); addr2 = Line 3; given = true; set_dot = false}.
 *)
type range = {
  addr1 : t option;
  addr2 : t;
  given : bool;
  (*s: [[Address.range]] other fields *)
  set_dot : bool;
  (*e: [[Address.range]] other fields *)
}
(*e: type [[Address.range]] *)
[@@deriving show]

(*****************************************************************************)
(* Parsing *)
(*****************************************************************************)
(*s: function [[Address.parse_delta]] *)
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
(*e: function [[Address.parse_delta]] *)
(*s: function [[Address.parse_relatives]] *)
let rec parse_relatives (base : t) (st : Parser.state) : t =
  match P.peek st with
  | T.Plus | T.Minus | T.Caret ->
      let d = parse_delta st in
      parse_relatives (Relative (base, d)) st
  | _ ->
      base
(*e: function [[Address.parse_relatives]] *)

(*s: function [[Address.parse_address]] *)
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
(*e: function [[Address.parse_address]] *)
(*s: function [[Address.parse_range]] *)
let parse_range (st : Parser.state) : range =
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
(*e: function [[Address.parse_range]] *)

(*****************************************************************************)
(* Evaluating *)
(*****************************************************************************)

(* TODO? need to pass [[a]] like in C with e.dot and adjust [[a]] as we go
 * like in C? so that /.../ and ?...? start from the right place?
 *)
(*s: function [[Address.eval_address]] *)
let rec eval_address (e : Env.t) (adr : t) : Env.lineno =
  match adr with
  | Current -> e.dot
  | Last -> e.dol
  | Line n -> n
  | Mark _ -> failwith "TODO: Mark"
  | SearchFwd _ | SearchBwd _ -> 
      let dir, re_str = 
        match adr with 
        | SearchFwd re -> 1, re
        | SearchBwd re -> -1, re
        | _ -> raise (Impossible "cases matched above")
      in
      (* less: opti: use SearchFwd of regex instead of str *)
      let re = Str.regexp re_str in
      (* starting point *)
      (* TODO: ed: need to be `a` instead like in C ? *)
      let a = e.dot in
      let b = a  in
      let rec aux (a : Env.lineno) : Env.lineno =
        let a = a + dir in
        let a =
          match () with
          (* wrap around start/end of buffer *)
          | _ when a <= 0 -> e.dol
          | _ when a > e.dol -> 1
          | _ -> a
        in
        match () with
        | _ when Commands.match_ e re a -> a
        (* back to starting point and nothing was found *)
        | _ when a = b ->
            Error.e_warn (spf "search for %s had no match" re_str)
        | _ ->
            aux a
      in
      aux a

  | Relative (x, n) -> eval_address e x + n
(*e: function [[Address.eval_address]] *)
(*s: function [[Address.eval_range]] *)
let eval_range (e : Env.t) (r : range) : Env.lineno * Env.lineno =
  Logs.debug (fun m -> m "range = %s" (show_range r));

  let addr2 = eval_address e r.addr2 in
  let addr1 =
    match r.addr1 with
    | None -> addr2
    | Some a -> eval_address e a
  in
  addr1, addr2
(*e: function [[Address.eval_range]] *)
(*e: Address.ml *)
