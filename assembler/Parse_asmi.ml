(* Copyright 2025 Yoann Padioleau, see copyright.txt *)
open Common
open Regexp_.Operators

module L = Location_cpp
module T = Token_asm
module A = Ast_asm
open Parser_asmi
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
      if i < Ast_asmi.nb_registers && i >=0
      then TRx x
      else Lexer_asm.error ("register number not valid")
  | T.TFx ((A.FR i) as x) -> 
      if i < Ast_asmi.nb_fregisters && i >=0
      then TFx x
      else Lexer_asm.error ("register number not valid")

  | T.TIDENT s ->
      (match s with
      (* instructions *)
(*
      | "AND" -> TARITH AND | "ORR" -> TARITH ORR | "EOR" -> TARITH EOR

      | "ADD" -> TARITH ADD | "SUB" -> TARITH SUB
      | "MUL" -> TARITH MUL | "DIV" -> TARITH DIV | "MOD" -> TARITH MOD
      | "SLL" -> TARITH SLL | "SRL" -> TARITH SRL | "SRA" -> TARITH SRA

      | "BIC" -> TARITH BIC
      | "ADC" -> TARITH ADC | "SBC" -> TARITH SBC
      | "RSB" -> TARITH RSB | "RSC" -> TARITH RSC

      | "MVN" -> TMVN
*)

      (* could move to Lexer_asm.mll and Ast_asm.virtual_instr *)
(*
      | "MOVW" -> TMOV A.Word
      | "MOVB" -> TMOV (A.Byte     A.S) | "MOVBU" -> TMOV (A.Byte     A.U)
      | "MOVH" -> TMOV (A.HalfWord A.S) | "MOVHU" -> TMOV (A.HalfWord A.U)

      | "B" -> TB | "BL" -> TBL
      | "CMP" -> TCMP CMP 
      | "TST" -> TCMP TST | "TEQ" -> TCMP TEQ | "CMN" -> TCMP CMN

      | "BEQ" -> TBx EQ | "BNE" -> TBx NE
      | "BGT" -> TBx (GT A.S) | "BLT" -> TBx (LT A.S)
      | "BGE" -> TBx (GE A.S) | "BLE" -> TBx (LE A.S)
      | "BHI" -> TBx (GT A.U) | "BLO" -> TBx (LT A.U) 
      | "BHS" -> TBx (GE A.U) | "BLS" -> TBx (LE A.U)
      | "BMI" -> TBx MI | "BPL" -> TBx PL 
      | "BVS" -> TBx VS | "BVC" -> TBx VC

      | "SWI" -> TSWI
      | "RFE" -> TRFE

      (* conditions *)
      | ".EQ" -> TCOND EQ | ".NE" -> TCOND NE
      | ".GT" -> TCOND (GT A.S)   | ".LT" -> TCOND (LT A.S) 
      | ".GE" -> TCOND (GE A.S)   | ".LE" -> TCOND (LE A.S)
      | ".HI" -> TCOND (GT A.U) | ".LO" -> TCOND (LT A.U)
      | ".HS" -> TCOND (GE A.U) | ".LS" -> TCOND (LE A.U)
      | ".MI" -> TCOND MI | ".PL" -> TCOND PL 
      | ".VS" -> TCOND VS | ".VC" -> TCOND VC

      (* less: special bits *)
      (* float, MUL, ... *)
      | "ADDF" -> TARITHF (ADD_, A.F)
      | "SUBF" -> TARITHF (SUB_, A.F)
      | "MULF" -> TARITHF (MUL_, A.F)
      | "DIVF" -> TARITHF (DIV_, A.F)
      | "ADDD" -> TARITHF (ADD_, A.D) 
      | "SUBD" -> TARITHF (SUB_, A.D)
      | "MULD" -> TARITHF (MUL_, A.D)
      | "DIVD" -> TARITHF (DIV_, A.D)

      | "CMPF" -> TCMPF A.F
      | "CMPD" -> TCMPF A.D
      (* advanced *)
      | "C" -> TC
      | _ when s =~ "^C\\([0-9]+\\)$" ->
            let i = int_of_string (Regexp_.matched1 s) in
            if i >= 0 && i <= 15
            then TCx (C i)
            else Lexer_asm.error ("register number not valid")
*)
      | _ -> TIDENT s
      )

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

let parse (caps : < Cap.open_in; .. >) (conf : Preprocessor.conf)
      (file : Fpath.t) :
    Ast_asmi.program = 
  let hooks = Parse_cpp.{
     lexer = token;
     parser = Parser_asmi.program;
     category = (fun t ->
       match t with
       | Parser_asmi.EOF -> Parse_cpp.Eof
       | Parser_asmi.TSharp -> Parse_cpp.Sharp
       | Parser_asmi.TIDENT s -> Parse_cpp.Ident s
        (* stricter: I forbid to have macros overwrite keywords *)
       | _ -> Parse_cpp.Other
     );
     eof = Parser_asmi.EOF;
  }
  in
  Parse_cpp.parse caps hooks conf file

(* Simpler code path; possibly useful in tests *)
let parse_no_cpp (chan : Chan.i) : Ast_asmi.program =
  L.line := 1;
  let lexbuf = Lexing.from_channel chan.ic in
  try 
    Parser_asmi.program token lexbuf, []
  with Parsing.Parse_error ->
      failwith (spf "Syntax error: line %d" !L.line)
