(*s: Parse.ml *)
(* Copyright 2016 Yoann Padioleau, see copyright.txt *)
open Stdcompat (* for |> *)
open Common

module R = Runtime

(*s: function [[Parse.error]] *)
let error s (curtok, curtokstr) =
  let t = R.cur () in
  let locstr =
    match t.R.file, t.R.iflag with
    | Some f, false -> spf "%s:%d: " f !(t.R.line)
    | Some f, true -> spf "%s: " f
    | None, false -> spf "%d: " !(t.R.line)
    | None, true -> ""
  in
  let tokstr = 
    match curtok with
    | Parser.TNewline -> ""
    | _ -> spf "token %s: " (curtokstr)
  in
  let str = spf "rc: %s%s%s" locstr tokstr s in

  (* todo: reset globals like lastdol, lastword *)
  (* less: error recovery, skip until next newline *)
  Status.setstatus s;

  (* less: nerror++; *)
  failwith str
(*e: function [[Parse.error]] *)

(*s: function [[Parse.parse_line]] *)
let parse_line lexbuf =
  let curtok = ref (Parser.EOF, "EOF") in

  Globals.skipnl := false;
  let got_skipnl_last_round = ref false in

  let lexfunc lexbuf =
    (* less: could do that in caller? would remove need for doprompt *)
    if !Prompt.doprompt
    then Prompt.pprompt ();

    let tok = ref (Lexer.token lexbuf) in

    if !got_skipnl_last_round then begin
      if !tok = Parser.TNewline 
      then begin
        let rec loop () =
          Prompt.pprompt ();
          tok := Lexer.token lexbuf;
          if !tok = Parser.TNewline
          then loop ()
        in
        loop ()
      end;
      got_skipnl_last_round := false;
    end;
    if !Globals.skipnl 
    then got_skipnl_last_round := true;
    Globals.skipnl := false;

    let s = Lexing.lexeme lexbuf in
    curtok := (!tok, s);
    (* todo: 
       - handle lastdol 
       - handle SUB
       - handle free caret insertion
    *)
    !tok 
    |> (fun tok -> 
        if !Flags.dump_tokens 
        then Logs.app (fun m -> m "%s" (Dumper.dump (tok,s)));
        tok)
  in
  try 
    Parser.rc lexfunc lexbuf
    |> (fun ast -> if !Flags.dump_ast then Logs.app (fun m -> m "%s" (Dumper_.s_of_line ast)); ast)
  with 
    | Parsing.Parse_error ->
        error "syntax error" !curtok
    | Lexer.Lexical_error s ->
        error (spf "lexical error, %s" s) !curtok
(*e: function [[Parse.parse_line]] *)
(*e: Parse.ml *)
