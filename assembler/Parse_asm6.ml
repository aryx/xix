(* Claude Code, Copyright (C) 2026 Yoann Padioleau, see copyright.txt *)
open Common

module L = Location_cpp
module T = Token_asm
module A = Ast_asm
open Parser_asm6
open Ast_asm6

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Lexer *)
(*****************************************************************************)
let token (lexbuf : Lexing.lexbuf) : Parser_asm6.token =
  let tok = Lexer_asm.token lexbuf in
  match tok with
  | T.TTEXT -> TTEXT
  | T.TGLOBL -> TGLOBL
  | T.TDATA -> TDATA
  | T.TWORD -> TWORD
  (* claude: amd64 has a real "RET" hardware instruction (0xc3) -- the
   * shared Lexer_asm.mll special-cases the raw string "RET" into this
   * shared T.TRET token regardless of arch (same as every other arch),
   * so this produces this grammar's own real-instruction TRET token
   * (Ast_asm6.ml's Ret), not a virtual one. NOP isn't wired in this
   * arch's grammar yet (goken's real amd64 NOP is 0x90, a genuine
   * instruction, just not needed by hello_linux_amd64.s) -- TNOP is
   * declared-but-unused, same shared convention as every other arch. *)
  | T.TRET -> TRET
  | T.TNOP -> TNOP
  | T.TEND -> TEND
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
  | T.TCOMMA -> TC
  | T.TDOLLAR -> TDOLLAR
  | T.TOPAR -> TOPAR
  | T.TCPAR -> TCPAR
  | T.TLBRACKET -> TLBRACKET
  | T.TRBRACKET -> TRBRACKET
  | T.TBANG -> TBANG
  | T.TLT -> TLT
  | T.TGT -> TGT
  | T.TPLUS -> TPLUS
  | T.TMINUS -> TMINUS
  | T.TMUL -> TMUL
  | T.TSLASH -> TSLASH
  | T.TMOD -> TMOD
  | T.TSharp -> TSharp
  | T.EOF -> EOF

  | T.TRx ((A.R i) as x) ->
      if i < Ast_asm6.nb_registers && i >= 0
      then TRx x
      else Lexer_asm.error ("register number not valid")
  | T.TFx ((A.FR i) as x) ->
      if i < Ast_asm6.nb_fregisters && i >= 0
      then TFx x
      else Lexer_asm.error ("register number not valid")

  | T.TIDENT s ->
      (match s with
      (* claude: goken's case shape "ADDQ/SUBQ/XORQ/ANDQ/ORQ $imm|Rs,Rd"
       * -- see Ast_asm6.ml's Arith comment. All four widths (Q/L/W/B)
       * are wired -- see Ast_asm6.ml's own `width` comment. *)
      | "ADDQ" -> TARITH (Q_, ADD)
      | "SUBQ" -> TARITH (Q_, SUB)
      | "XORQ" -> TARITH (Q_, XOR)
      | "ANDQ" -> TARITH (Q_, AND)
      | "ORQ" -> TARITH (Q_, OR)
      | "CMPQ" -> TCMP Q_
      | "ADDL" -> TARITH (L_, ADD)
      | "SUBL" -> TARITH (L_, SUB)
      | "XORL" -> TARITH (L_, XOR)
      | "ANDL" -> TARITH (L_, AND)
      | "ORL" -> TARITH (L_, OR)
      | "CMPL" -> TCMP L_
      | "ADDW" -> TARITH (W_, ADD)
      | "SUBW" -> TARITH (W_, SUB)
      | "XORW" -> TARITH (W_, XOR)
      | "ANDW" -> TARITH (W_, AND)
      | "ORW" -> TARITH (W_, OR)
      | "CMPW" -> TCMP W_
      | "ADDB" -> TARITH (B_, ADD)
      | "SUBB" -> TARITH (B_, SUB)
      | "XORB" -> TARITH (B_, XOR)
      | "ANDB" -> TARITH (B_, AND)
      | "ORB" -> TARITH (B_, OR)
      | "CMPB" -> TCMP B_

      (* claude: real x86 aliases -- SHL and SAL are the exact same
       * opcode (ext=4), both spelled out as separate optab.c entries
       * in goken with byte-for-byte identical rows -- see Ast_asm6.ml's
       * `shift_opcode` comment. *)
      | "SHLQ" | "SALQ" -> TSHIFT (Q_, SHL)
      | "SHRQ" -> TSHIFT (Q_, SHR)
      | "SARQ" -> TSHIFT (Q_, SAR)
      | "SHLL" | "SALL" -> TSHIFT (L_, SHL)
      | "SHRL" -> TSHIFT (L_, SHR)
      | "SARL" -> TSHIFT (L_, SAR)
      | "SHLW" | "SALW" -> TSHIFT (W_, SHL)
      | "SHRW" -> TSHIFT (W_, SHR)
      | "SARW" -> TSHIFT (W_, SAR)
      | "SHLB" | "SALB" -> TSHIFT (B_, SHL)
      | "SHRB" -> TSHIFT (B_, SHR)
      | "SARB" -> TSHIFT (B_, SAR)

      | "MOVQ" -> TMOV Q_
      | "MOVL" -> TMOV L_
      | "MOVW" -> TMOV W_
      | "MOVB" -> TMOV B_
      | "LEAQ" -> TLEA
      | "CALL" -> TCALL
      (* claude: goken's real amd64 condition codes (optab.c's
       * AJEQ/AJNE/.../AJLS) -- see Ast_asm6.ml's `condition` comment
       * for the signed/unsigned split. *)
      | "JMP" -> TJMP
      | "JEQ" -> TJcc EQ | "JNE" -> TJcc NE
      | "JLT" -> TJcc (LT A.S) | "JGE" -> TJcc (GE A.S)
      | "JGT" -> TJcc (GT A.S) | "JLE" -> TJcc (LE A.S)
      | "JCS" -> TJcc (LT A.U) | "JCC" -> TJcc (GE A.U)
      | "JHI" -> TJcc (GT A.U) | "JLS" -> TJcc (LE A.U)
      | "RET" -> TRET
      | "SYSCALL" -> TSYSCALL

      (* claude: SSE only (single- and double-precision), no x87 -- see
       * Ast_asm6.ml's own "Scope so far" note. goken's optab.c lists
       * ADDSD/SUBSD/MULSD/DIVSD (and their SS-suffixed siblings) all
       * sharing the exact same `yxm` table (only the final opcode byte
       * differs *per operation*, not per precision -- see
       * Codegen6.ml's `arithf_opcode_byte`). *)
      | "MOVSD" -> TMOVF A.D
      | "ADDSD" -> TARITHF (FADD, A.D)
      | "SUBSD" -> TARITHF (FSUB, A.D)
      | "MULSD" -> TARITHF (FMUL, A.D)
      | "DIVSD" -> TARITHF (FDIV, A.D)
      | "UCOMISD" -> TUCOMISF A.D
      | "CVTSQ2SD" -> TCVTINTTOF A.D
      | "CVTTSD2SQ" -> TCVTFTOINT A.D
      | "MOVSS" -> TMOVF A.F
      | "ADDSS" -> TARITHF (FADD, A.F)
      | "SUBSS" -> TARITHF (FSUB, A.F)
      | "MULSS" -> TARITHF (FMUL, A.F)
      | "DIVSS" -> TARITHF (FDIV, A.F)
      | "UCOMISS" -> TUCOMISF A.F
      | "CVTSQ2SS" -> TCVTINTTOF A.F
      | "CVTTSS2SQ" -> TCVTFTOINT A.F

      (* claude: named low registers -- goken's real 6a/lex.c has a
       * dedicated register-name hash table for these (a.h/D_AL..D_DI
       * etc), unlike the shared Lexer_asm.mll's generic "R" + digit
       * rule (which already covers R8-R15 directly, no mapping needed
       * here -- see Ast_asm6.ml's prelude). SP is deliberately absent
       * here: it's the shared TSP token (see Parser_asm6.mly's `reg`
       * comment), not a plain TIDENT. BP isn't wired yet. *)
      | "AX" -> TRx (A.R 0)
      | "CX" -> TRx (A.R 1)
      | "DX" -> TRx (A.R 2)
      | "BX" -> TRx (A.R 3)
      | "SI" -> TRx (A.R 6)
      | "DI" -> TRx (A.R 7)

      (* claude: XMM registers -- goken's own lex.c lists these as 16
       * individual named tokens too (not a generic "letter+digit"
       * rule), so this port mirrors that here rather than adding a
       * generic "X"+digit case to the shared Lexer_asm.mll (which
       * would ripple across every other arch's own grammar for no
       * benefit -- see Parser_asm6.mly's own TXx comment). *)
      | "X0" -> TXx (X 0) | "X1" -> TXx (X 1)
      | "X2" -> TXx (X 2) | "X3" -> TXx (X 3)
      | "X4" -> TXx (X 4) | "X5" -> TXx (X 5)
      | "X6" -> TXx (X 6) | "X7" -> TXx (X 7)
      | "X8" -> TXx (X 8) | "X9" -> TXx (X 9)
      | "X10" -> TXx (X 10) | "X11" -> TXx (X 11)
      | "X12" -> TXx (X 12) | "X13" -> TXx (X 13)
      | "X14" -> TXx (X 14) | "X15" -> TXx (X 15)

      | _ -> TIDENT s
      )

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

let parse (caps : < Cap.open_in; .. >) (conf : Preprocessor.conf)
      (file : Fpath.t) :
    Ast_asm6.program =
  let hooks = Parse_cpp.{
     lexer = token;
     parser = Parser_asm6.program;
     category = (fun t ->
       match t with
       | Parser_asm6.EOF -> Parse_cpp.Eof
       | Parser_asm6.TSharp -> Parse_cpp.Sharp
       | Parser_asm6.TIDENT s -> Parse_cpp.Ident s
       | _ -> Parse_cpp.Other
     );
     eof = Parser_asm6.EOF;
  }
  in
  Parse_cpp.parse caps hooks conf file

(* Simpler code path; possibly useful in tests *)
let parse_no_cpp (chan : Chan.i) : Ast_asm6.program =
  L.line := 1;
  let lexbuf = Lexing.from_channel chan.ic in
  try
    Parser_asm6.program token lexbuf, []
  with Parsing.Parse_error ->
      failwith (spf "Syntax error: line %d" !L.line)
