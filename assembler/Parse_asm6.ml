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
      (* claude: goken's case shape "ADDQ/SUBQ/XORQ $imm|Rs,Rd" -- see
       * Ast_asm6.ml's Arith comment. Only the Q (64-bit/quadword)
       * suffix is wired -- see this file's/Ast_asm6.ml's own "Scope
       * for this first checkpoint" note. *)
      | "ADDQ" -> TARITH ADD
      | "SUBQ" -> TARITH SUB
      | "XORQ" -> TARITH XOR

      | "MOVQ" -> TMOV Q_
      | "LEAQ" -> TLEA
      | "CALL" -> TCALL
      | "RET" -> TRET
      | "SYSCALL" -> TSYSCALL

      (* claude: named low registers -- goken's real 6a/lex.c has a
       * dedicated register-name hash table for these (a.h/D_AL..D_DI
       * etc), unlike the shared Lexer_asm.mll's generic "R" + digit
       * rule (which already covers R8-R15 unchanged, once wired --
       * see Ast_asm6.ml's prelude). SP is deliberately absent here:
       * it's the shared TSP token (see Parser_asm6.mly's `reg`
       * comment), not a plain TIDENT. BP/R8-R15 aren't wired yet. *)
      | "AX" -> TRx (A.R 0)
      | "CX" -> TRx (A.R 1)
      | "DX" -> TRx (A.R 2)
      | "BX" -> TRx (A.R 3)
      | "SI" -> TRx (A.R 6)
      | "DI" -> TRx (A.R 7)

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
