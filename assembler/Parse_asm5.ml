(* Copyright 2015, 2016 Yoann Padioleau, see copyright.txt *)
open Common

module L = Location_cpp
(* for fields access for ocaml-light *)
open Parse_cpp
open Chan
module T = Token_asm
open Parser_asm5
open Ast_asm5

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Lexer *)
(*****************************************************************************)

let token (lexbuf : Lexing.lexbuf) : Parser_asm5.token =
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
      if i <= 15 && i >=0
      then TRx x
      else Lexer_asm.error ("register number not valid")
  | T.TFx ((A.F i) as x) -> 
      if i <= 15 && i >=0
      then TFx x
      else Lexer_asm.error ("register number not valid")
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
  | T.TIDENT s ->
      (match s with
      (* instructions *)
      | "AND" -> TARITH AND | "ORR" -> TARITH ORR | "EOR" -> TARITH EOR

      | "ADD" -> TARITH ADD | "SUB" -> TARITH SUB
      | "MUL" -> TARITH MUL | "DIV" -> TARITH DIV | "MOD" -> TARITH MOD
      | "SLL" -> TARITH SLL | "SRL" -> TARITH SRL | "SRA" -> TARITH SRA

      | "BIC" -> TARITH BIC
      | "ADC" -> TARITH ADC | "SBC" -> TARITH SBC
      | "RSB" -> TARITH RSB | "RSC" -> TARITH RSC

      | "MVN" -> TMVN

      (* could move to Lexer_asm.mll and Ast_asm.virtual_instr *)
      | "MOVW" -> TMOV Word
      | "MOVB" -> TMOV (Byte     Signed) | "MOVBU" -> TMOV (Byte     Unsigned)
      | "MOVH" -> TMOV (HalfWord Signed) | "MOVHU" -> TMOV (HalfWord Unsigned)

      | "B" -> TB | "BL" -> TBL
      | "CMP" -> TCMP CMP 
      | "TST" -> TCMP TST | "TEQ" -> TCMP TEQ | "CMN" -> TCMP CMN

      | "BEQ" -> TBx EQ | "BNE" -> TBx NE
      | "BGT" -> TBx (GT Signed) | "BLT" -> TBx (LT Signed)
      | "BGE" -> TBx (GE Signed) | "BLE" -> TBx (LE Signed)
      | "BHI" -> TBx (GT Unsigned) | "BLO" -> TBx (LT Unsigned) 
      | "BHS" -> TBx (GE Unsigned) | "BLS" -> TBx (LE Unsigned)
      | "BMI" -> TBx MI | "BPL" -> TBx PL 
      | "BVS" -> TBx VS | "BVC" -> TBx VC

      | "SWI" -> TSWI
      | "RFE" -> TRFE

      (* conditions *)
      | ".EQ" -> TCOND EQ | ".NE" -> TCOND NE
      | ".GT" -> TCOND (GT Signed)   | ".LT" -> TCOND (LT Signed) 
      | ".GE" -> TCOND (GE Signed)   | ".LE" -> TCOND (LE Signed)
      | ".HI" -> TCOND (GT Unsigned) | ".LO" -> TCOND (LT Unsigned)
      | ".HS" -> TCOND (GE Unsigned) | ".LS" -> TCOND (LE Unsigned)
      | ".MI" -> TCOND MI | ".PL" -> TCOND PL 
      | ".VS" -> TCOND VS | ".VC" -> TCOND VC

      (* less: special bits *)
      (* less: float, MUL, ... *)


      | _ -> TIDENT s
      )

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

let parse (caps : < Cap.open_in; .. >) (conf : Preprocessor.conf) (file : Fpath.t) : Ast_asm5.program = 
  let hooks = { Parse_cpp.
     lexer = token;
     parser = Parser_asm5.program;
     category = (fun t ->
       match t with
       | Parser_asm5.EOF -> Parse_cpp.Eof
       | Parser_asm5.TSharp -> Parse_cpp.Sharp
       | Parser_asm5.TIDENT s -> Parse_cpp.Ident s
        (* stricter: I forbid to have macros overwrite keywords *)
       | _ -> Parse_cpp.Other
     );
     eof = Parser_asm5.EOF;
  }
  in
  Parse_cpp.parse caps hooks conf file


(* Simpler code path; possibly useful in tests *)
let parse_no_cpp (chan : Chan.i) : Ast_asm5.program =
  L.line := 1;
  let lexbuf = Lexing.from_channel chan.ic in
  try 
    Parser_asm5.program token lexbuf
  with Parsing.Parse_error ->
      failwith (spf "Syntax error: line %d" !L.line)

