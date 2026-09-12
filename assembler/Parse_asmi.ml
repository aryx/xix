(* Copyright 2025 Yoann Padioleau, see copyright.txt *)
open Common
open Regexp.Operators

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
      (* claude: this whole dispatch was a copy-pasted-then-commented
       * ARM template (Parse_asm5.ml's keyword table, complete with
       * ARM-only mnemonics like SWI/RFE/BHI/CMPF that don't even
       * exist on RISC-V) -- none of it had been adapted to Ast_asmi's
       * actual grammar (Parser_asmi.mly), which uses TMOVE1/TMOVE2
       * (not ARM's single TMOV) and TSYSCALL (not TSWI), among other
       * differences. Wiring up only what's currently exercised by a
       * fixture (MOVW, ECALL); the rest (MOVB/MOVH/MOVBU/MOVHU,
       * arithmetic, branches, JAL/JALR) is real backlog, not yet
       * done -- see docs/claude_notes/notes_riscv_port_plan.txt.
       *)

      (* MOVW $imm,R / MOVW $sym(SB),R / MOVW R,R / MOVW mem,R / MOVW R,mem
       * all go through Move2's W__ (word) case, the same as ARM/MIPS's
       * unified Move2 -- unlike MOVB/MOVH, which need Move1 for their
       * sign/zero-extension distinction on loads (not wired yet).
       *)
      | "MOVW" -> TMOVE2 W__

      | "ECALL" -> TSYSCALL

      (* claude: register-register arithmetic (case 0) and shift-
       * immediate (case 1) -- the RV32-native forms only (`w option`
       * = None), not the explicit-32-bit-on-RV64 *W variants
       * (ADDW/SLLW/etc, a separate opcode family -- see
       * oprrr_arith_opcode's comment in Codegeni.ml). *)
      | "ADD" -> TARITH (ADD None) | "SUB" -> TARITH (SUB None)
      | "SLL" -> TARITH (SLL None) | "SRL" -> TARITH (SRL None)
      | "SRA" -> TARITH (SRA None)
      | "SLT" -> TARITH (SLT A.S) | "SLTU" -> TARITH (SLT A.U)
      | "XOR" -> TARITH XOR | "OR" -> TARITH OR | "AND" -> TARITH AND

      (* claude: MULH/MULHSU/MULHU aren't wired -- Ast_asmi's
       * mul_opcode has no constructor for them yet (`MUL` alone,
       * with a standing "TODO: lots of MUL" -- see the AST); left
       * as a follow-up rather than guessed at. *)
      | "MUL" -> TMULOP MUL
      | "DIV" -> TMULOP (DIV (None, A.S)) | "DIVU" -> TMULOP (DIV (None, A.U))
      | "REM" -> TMULOP (REM (None, A.S)) | "REMU" -> TMULOP (REM (None, A.U))

      (* claude: LE/GT have no direct RISC-V hardware branch (only
       * BEQ/BNE/BLT/BGE/BLTU/BGEU exist -- goken's own assembler
       * doesn't accept "BLE"/"BGT" mnemonics either, they'd need an
       * operand-swapping pseudo-op rewrite this session doesn't
       * add), so only EQ/NE/LT/GE (both signs) are wired here. *)
      | "BEQ" -> TB EQ | "BNE" -> TB NE
      | "BLT" -> TB (LT A.S) | "BGE" -> TB (GE A.S)
      | "BLTU" -> TB (LT A.U) | "BGEU" -> TB (GE A.U)

      | "JMP" -> TJMP | "JAL" -> TJAL

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
