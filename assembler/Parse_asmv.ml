(* Copyright 2025 Yoann Padioleau, see copyright.txt *)
open Common
open Regexp_.Operators

module L = Location_cpp
(* for fields access for ocaml-light *)
open Parse_cpp
open Chan
module T = Token_asm
open Parser_asmv
open Ast_asmv

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Lexer *)
(*****************************************************************************)

let token (lexbuf : Lexing.lexbuf) : Parser_asmv.token =
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
  | T.TRx ((A.R i) as x) -> 
      if i <= 31 && i >=0
      then TRx x
      else Lexer_asm.error ("register number not valid")
  | T.TFx ((A.F i) as x) -> 
      if i <= 31 && i >=0
      then TFx x
      else Lexer_asm.error ("register number not valid")
  | T.TINT i -> TINT i
  | T.TFLOAT f -> TFLOAT f
  | T.TSTRING s -> TSTRING s
  | T.TSEMICOLON i -> TSEMICOLON i
  | T.TCOLON -> TCOLON 
  | T.TDOT -> TDOT 
  | T.TCOMMA-> TC
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
  | T.TIDENT s ->
      (match s with
      (* instructions *)
      | "AND" -> TARITH AND | "OR" -> TARITH OR | "XOR" -> TARITH XOR 
      | "NOR" -> TNOR

      | "ADD" -> TARITH (ADD (W, S)) 
      | "ADDU" -> TARITH (ADD (W, U))
      | "ADDV" -> TARITH (ADD (V, S))
      | "ADDVU"  -> TARITH (ADD (V, U))
      | "SUB" -> TARITH (SUB (W, S)) 
      | "SUBU" -> TARITH (SUB (W, U))
      | "SUBV" -> TARITH (SUB (V, S))
      | "SUBVU"  -> TARITH (SUB (V, U))

      | "SLL" -> TARITH (SLL W)
      | "SLLV" -> TARITH (SLL V)
      | "SRL" -> TARITH (SRL W)
      | "SRLV" -> TARITH (SRL V)
      | "SRA" -> TARITH (SRA W)
      | "SRAV" -> TARITH (SRA V)

      | "SGT" -> TARITH (SGT S)
      | "SGTU" -> TARITH (SGT U)

      | "MUL" -> TMULOP (MUL (W, S)) 
      | "MULU" -> TMULOP (MUL (W, U))
      | "MULV" -> TMULOP (MUL (V, S))
      | "MULVU"  -> TMULOP (MUL (V, U))
      | "DIV" -> TMULOP (DIV (W, S)) 
      | "DIVU" -> TMULOP (DIV (W, U))
      | "DIVV" -> TMULOP (DIV (V, S))
      | "DIVVU"  -> TMULOP (DIV (V, U))

      | "REM" -> TMULOP (REM S)
      | "REMU" -> TMULOP (REM U)
    
      | "JMP" -> TJMP
      | "JAL" -> TJAL

      | "SYSCALL" -> TSYSCALL
      | "BREAK" -> TBREAK
      | "RFE" -> TRFE

      | "TLBP" -> TTLB P
      | "TLBR" -> TTLB R
      | "TLBWI" -> TTLB WI
      | "TLBWR" -> TTLB WR

      (* advanced *)
      | "M" -> TM
      | _ when s =~ "^M\\([0-9]+\\)$" ->
            let i = int_of_string (Regexp_.matched1 s) in
            if i >= 0 && i <= 31
            then TMx (M i)
            else Lexer_asm.error ("register number not valid")
      | "FCR" -> TFCR
      | _ when s =~ "^FCR\\([0-9]+\\)$" ->
            let i = int_of_string (Regexp_.matched1 s) in
            if i >= 0 && i <= 31
            then TFCRx (FCR i)
            else Lexer_asm.error ("register number not valid")
      | _ -> TIDENT s
      )

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

let parse (caps : < Cap.open_in; .. >) (conf : Preprocessor.conf) (file : Fpath.t) : Ast_asmv.program = 
  let hooks = { Parse_cpp.
     lexer = token;
     parser = Parser_asmv.program;
     category = (fun t ->
       match t with
       | Parser_asmv.EOF -> Parse_cpp.Eof
       | Parser_asmv.TSharp -> Parse_cpp.Sharp
       | Parser_asmv.TIDENT s -> Parse_cpp.Ident s
        (* stricter: I forbid to have macros overwrite keywords *)
       | _ -> Parse_cpp.Other
     );
     eof = Parser_asmv.EOF;
  }
  in
  Parse_cpp.parse caps hooks conf file


(* Simpler code path; possibly useful in tests *)
let parse_no_cpp (chan : Chan.i) : Ast_asmv.program =
  L.line := 1;
  let lexbuf = Lexing.from_channel chan.ic in
  try 
    Parser_asmv.program token lexbuf
  with Parsing.Parse_error ->
      failwith (spf "Syntax error: line %d" !L.line)
