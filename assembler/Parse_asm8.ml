(* Copyright 2025 Yoann Padioleau, see copyright.txt *)
open Common

module L = Location_cpp
(* for fields access for ocaml-light *)
open Parse_cpp
open Chan
module T = Token_asm
open Parser_asm8
open Ast_asmi

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Lexer *)
(*****************************************************************************)

let token (lexbuf : Lexing.lexbuf) : Parser_asmi.token =
  let tok = Lexer_asm.token lexbuf in

  match tok with
  | T.TTEXT -> TTEXT
  | T.TGLOBL -> TGLOBL
  | T.TDATA -> TDATA
  | T.TWORD -> TWORD
  | T.TRET -> TRET
  | T.TNOP -> TNOP
  | T.TR -> TR
  | T.TF -> TF
  | T.TPC -> TPC
  | T.TSB -> TSB
  | T.TFP -> TFP
  | T.TSP -> TSP
  | T.TINT i -> TINT i
  | T.TFLOAT f -> TFLOAT f
  | T.TSTRING s -> TSTRING s
  | T.TSEMICOLON i -> TSEMICOLON i
  | T.TCOLON -> TCOLON 
  | T.TDOT -> TDOT 
  | T.TCOMMA-> TCOMMA
  | T.TDOLLAR-> TDOLLAR
  | T.TOPAR-> TOPAR
  | T.TCPAR-> TCPAR
  | T.TPLUS-> TPLUS
  | T.TMINUS-> TMINUS
  | T.TMUL-> TMUL
  | T.TSLASH-> TSLASH
  | T.TMOD-> TMOD
  | T.TSharp-> TSharp
  | T.EOF-> EOF

  | T.TRx ((A.R i) as x) -> 
      if i <= 31 && i >=0
      then TRx x
      else Lexer_asm.error ("register number not valid")
  | T.TFx ((A.F i) as x) -> 
      if i <= 31 && i >=0
      then TFx x
      else Lexer_asm.error ("register number not valid")

  | T.TIDENT s ->
      (match s with
      (* instructions *)

      | _ -> TIDENT s
      )

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let parse (caps : < Cap.open_in; .. >) (conf : Preprocessor.conf) (file : Fpath.t) : Ast_asm8.program = 
  let hooks = { Parse_cpp.
     lexer = token;
     parser = Parser_asm8.program;
     category = (fun t ->
       match t with
       | Parser_asm8.EOF -> Parse_cpp.Eof
       | Parser_asm8.TSharp -> Parse_cpp.Sharp
       | Parser_asm8.TIDENT s -> Parse_cpp.Ident s
        (* stricter: I forbid to have macros overwrite keywords *)
       | _ -> Parse_cpp.Other
     );
     eof = Parser_asm8.EOF;
  }
  in
  Parse_cpp.parse caps hooks conf file
