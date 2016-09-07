(* Copyright 2016 Yoann Padioleau, see copyright.txt *)
open Common

module R = Runtime

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
  (* less:
     nerror++;
     setvar("status", newword(m, nil));
  *)

  failwith str
   



let parse_line lexbuf =
  let curtok = ref (Parser.EOF, "EOF") in
  Globals.skipnl := false;
  let got_skipnl_last_round = ref false in

  let rec lexfunc lexbuf =
    (* todo: call pprompt if doprompt *)
    (* todo: have different state after having parsed a $?*)

    let tok = ref (Lexer.token lexbuf) in

    if !got_skipnl_last_round then begin
      (match !tok with
      | Parser.TNewline -> 
        let rec loop () =
          tok := Lexer.token lexbuf;
          if !tok = Parser.TNewline
          then loop ()
        in
        loop ()
      | _ -> ()
      );
      got_skipnl_last_round := false;
    end;
    if !Globals.skipnl 
    then got_skipnl_last_round := true;
    Globals.skipnl := false;


    let s = Lexing.lexeme lexbuf in
    curtok := (!tok, s);
    (* todo: 
       - handle lastdol 
       - call pprompt() if TNewline or TBackslashNewline
       - handle SUB
       - handle free caret insertion
    *)
    !tok 
    |> (fun tok -> if !Flags.dump_tokens then pr2_gen (tok,s) ; tok)
  in

    try 
      Parser.rc lexfunc lexbuf
      |> (fun ast -> if !Flags.dump_ast then pr2 (Dumper.s_of_line ast); ast)
    with 
      | Parsing.Parse_error ->
          error "syntax error" !curtok
      | Lexer.Lexical_error s ->
          error (spf "lexical error, %s" s) !curtok

