(* Copyright 2025 Yoann Padioleau, see copyright.txt *)
open Common
open Regexp_.Operators

module L = Location_cpp
module T = Token_asm
module A = Ast_asm
(* for fields access for ocaml-light *)
open Parse_cpp
open Chan
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
      | "AND" -> TARITH AND | "OR" -> TARITH OR | "XOR" -> TARITH XOR
      | "NOR" -> TNOR

      | "ADD" -> TARITH (ADD (W, A.S)) | "ADDU" -> TARITH (ADD (W, A.U))
      | "ADDV" -> TARITH (ADD (V, A.S))| "ADDVU"  -> TARITH (ADD (V, A.U))
      | "SUB" -> TARITH (SUB (W, A.S)) | "SUBU" -> TARITH (SUB (W, A.U))
      | "SUBV" -> TARITH (SUB (V, A.S)) | "SUBVU"  -> TARITH (SUB (V, A.U))

      | "SLL" -> TARITH (SLL W) | "SLLV" -> TARITH (SLL V)
      | "SRL" -> TARITH (SRL W) | "SRLV" -> TARITH (SRL V)
      | "SRA" -> TARITH (SRA W) | "SRAV" -> TARITH (SRA V)

      | "SGT" -> TARITH (SGT A.S) | "SGTU" -> TARITH (SGT A.U)

      | "MUL" -> TMULOP (MUL (W, A.S))  | "MULU" -> TMULOP (MUL (W, A.U))
      | "MULV" -> TMULOP (MUL (V, A.S)) | "MULVU"  -> TMULOP (MUL (V, A.U))
      | "DIV" -> TMULOP (DIV (W, A.S))  | "DIVU" -> TMULOP (DIV (W, A.U))
      | "DIVV" -> TMULOP (DIV (V, A.S)) | "DIVVU"  -> TMULOP (DIV (V, A.U))

      | "REM" -> TMULOP (REM A.S) | "REMU" -> TMULOP (REM A.U)
    
      | "JMP" -> TJMP | "JAL" -> TJAL

      | "BEQ" -> TBEQ | "BNE" -> TBNE

      | "BGEZ" -> TB GEZ
      | "BGEZAL" -> TB GEZAL
      | "BGTZ" -> TB GTZ
      | "BLEZ" -> TB LEZ
      | "BLTZ" -> TB LTZ
      | "BLTZAL" -> TB LTZAL

      | "SYSCALL" -> TSYSCALL
      | "BREAK" -> TBREAK
      | "RFE" -> TRFE

      | "TLBP" -> TTLB P_ | "TLBR" -> TTLB R_
      | "TLBWI" -> TTLB WI | "TLBWR" -> TTLB WR

      | "MOVB" -> TMOVE1 (B_ A.S) | "MOVBU" -> TMOVE1 (B_ A.U)
      | "MOVH" -> TMOVE1 (H_ A.S) | "MOVHU" -> TMOVE1 (H_ A.U)
      | "MOVWL" -> TMOVE1 (W_ Le) | "MOVWR" -> TMOVE1 (W_ Ri)
      | "MOVVL" -> TMOVE1 (V_ Le) | "MOVVR" -> TMOVE1 (V_ Ri)

      | "MOVW" -> TMOVE2 W__
      | "MOVV" -> TMOVE2 V__

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
(* Entry point *)
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
