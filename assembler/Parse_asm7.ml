(* Claude Code, Copyright (C) 2026 Yoann Padioleau, see copyright.txt *)
open Common

module L = Location_cpp
module T = Token_asm
module A = Ast_asm
open Parser_asm7
open Ast_asm7

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Lexer *)
(*****************************************************************************)
let token (lexbuf : Lexing.lexbuf) : Parser_asm7.token =
  let tok = Lexer_asm.token lexbuf in
  match tok with
  | T.TTEXT -> TTEXT
  | T.TGLOBL -> TGLOBL
  | T.TDATA -> TDATA
  | T.TWORD -> TWORD
  (* claude: ARM64 has a real "RET" hardware instruction (unlike
   * ARM32/MIPS/RISC-V, where RET is purely a compiler-facing virtual
   * instr expanded by Rewrite{5,v,i}.ml) -- but the shared Lexer_asm.mll
   * still special-cases the raw string "RET" into this shared T.TRET
   * token regardless of arch, so this dispatch produces this grammar's
   * own real-instruction TRET token (see Ast_asm7.ml's RET/Parser_asm7.mly's
   * "TRET"/"TRET reg" productions), not a virtual one. *)
  | T.TRET -> TRET
  | T.TEND -> TEND
  (* claude: goken's ARM64 does have a real NOP instruction (HINT #0
   * encoding, LTYPEQ) but it's not wired in Parser_asm7.mly yet -- this
   * mapping only exists to satisfy match exhaustiveness (same
   * declared-but-unused pattern ARM32/RISC-V already have for their own
   * virtual NOP). *)
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
      if i < Ast_asm7.nb_registers && i >= 0
      then TRx x
      else Lexer_asm.error ("register number not valid")
  | T.TFx ((A.FR i) as x) ->
      if i < Ast_asm7.nb_fregisters && i >= 0
      then TFx x
      else Lexer_asm.error ("register number not valid")

  | T.TIDENT s ->
      (match s with
      (* claude: case 1 -- register-register-or-immediate arith. AND's
       * OCaml constructor is `AND_` (a plain `AND` would shadow
       * Stdlib/Common's own boolean `&&`-family identifier -- same
       * "trailing underscore to dodge a keyword clash" convention used
       * elsewhere in this codebase, e.g. Ast_asmi.ml's `W_`/`H_`). *)
      | "ADD" -> TARITH ADD | "SUB" -> TARITH SUB
      | "AND" -> TARITH AND_ | "ORR" -> TARITH ORR
      | "EOR" -> TARITH EOR | "BIC" -> TARITH BIC

      (* claude: the *W-suffixed 32-bit-view forms -- see Ast_asm7.ml's
       * arith_opcode comment for why these are sibling constructors
       * rather than a separate width field. *)
      | "ADDW" -> TARITH ADDW | "SUBW" -> TARITH SUBW
      | "ANDW" -> TARITH ANDW | "ORRW" -> TARITH ORRW
      | "EORW" -> TARITH EORW | "BICW" -> TARITH BICW

      (* claude: case 8 (shift by immediate, bitfield-move encoding) /
       * case 9 (shift by register, simple oprrr encoding) -- same
       * mnemonic either way, dispatched by operand shape at codegen
       * time (Codegen7.ml). *)
      | "LSL" -> TSHIFT LSL | "LSR" -> TSHIFT LSR
      | "ASR" -> TSHIFT ASR | "ROR" -> TSHIFT ROR
      | "LSLW" -> TSHIFT LSLW | "LSRW" -> TSHIFT LSRW
      | "ASRW" -> TSHIFT ASRW | "RORW" -> TSHIFT RORW

      (* claude: case 7 -- CMP/CMN, no destination register. *)
      | "CMP" -> TCMP CMP | "CMN" -> TCMP CMN
      | "CMPW" -> TCMP CMPW | "CMNW" -> TCMP CMNW

      (* claude: case 15's simple 3-operand MUL only -- see
       * ArithMul's comment in Ast_asm7.ml for what's deferred. *)
      | "MUL" -> TMULOP MUL
      | "MULW" -> TMULOP MULW

      (* claude: case 3 -- MOV/MOVB/MOVBU/MOVH/MOVHU/MOVW/MOVWU, one
       * grammar shape ("gen,gen") dispatched by operand type at
       * codegen time, matching goken's own a.y (LTYPE3 covers all of
       * these under one production). Bare "MOV" is the 64-bit
       * (doubleword) form. *)
      | "MOV" -> TMOV X_
      | "MOVB" -> TMOV (B_ A.S) | "MOVBU" -> TMOV (B_ A.U)
      | "MOVH" -> TMOV (H_ A.S) | "MOVHU" -> TMOV (H_ A.U)
      | "MOVW" -> TMOV (W_ A.S) | "MOVWU" -> TMOV (W_ A.U)

      (* claude: float<->float register move / float<->memory (FS_/
       * FD_) and int<->float conversion (SCVTF_S/SCVTF_D/FCVTZS_S/
       * FCVTZS_D) -- all dispatched through the same "gen,gen" TMOV
       * production as ordinary MOV, see Ast_asm7.ml's Move/move_size
       * comment. *)
      | "FMOVS" -> TMOV FS_ | "FMOVD" -> TMOV FD_
      | "SCVTFS" -> TMOV SCVTF_S | "SCVTFD" -> TMOV SCVTF_D
      | "FCVTZSS" -> TMOV FCVTZS_S | "FCVTZSD" -> TMOV FCVTZS_D

      (* claude: case 54 -- dyadic float arith, both precisions. *)
      | "FADDS" -> TFARITH FADDS | "FADDD" -> TFARITH FADDD
      | "FSUBS" -> TFARITH FSUBS | "FSUBD" -> TFARITH FSUBD
      | "FMULS" -> TFARITH FMULS | "FMULD" -> TFARITH FMULD
      | "FDIVS" -> TFARITH FDIVS | "FDIVD" -> TFARITH FDIVD

      (* claude: case 56 -- float compare, both precisions (the
       * "signaling" FCMPES/FCMPED variants aren't wired, same
       * "narrower but real" scoping as elsewhere in this file). *)
      | "FCMPS" -> TFCMP FCMPS | "FCMPD" -> TFCMP FCMPD

      (* claude: case 51 -- memory/instruction barriers (see
       * Ast_asm7.ml's Barrier comment for why bare NOP and HINT
       * aren't wired here). *)
      | "DMB" -> TDMB DMB_ | "DSB" -> TDMB DSB_ | "ISB" -> TDMB ISB_

      (* claude: case 5/6 -- unconditional branch/call, direct or
       * indirect through a register. *)
      | "B" -> TB | "BL" -> TBL

      (* claude: case 7 (branch variant) -- BEQ/BNE/...; BCS/BHS and
       * BCC/BLO are goken's own synonym pairs for the same condition
       * (unsigned GE/LT respectively), both wired to the identical
       * AST value here, same as ARM32's Ast_asm5.condition sharing one
       * GE/LT-of-sign constructor for HS/LO. *)
      | "BEQ" -> TBx EQ | "BNE" -> TBx NE
      | "BCS" -> TBx (GE A.U) | "BHS" -> TBx (GE A.U)
      | "BCC" -> TBx (LT A.U) | "BLO" -> TBx (LT A.U)
      | "BMI" -> TBx MI | "BPL" -> TBx PL
      | "BVS" -> TBx VS | "BVC" -> TBx VC
      | "BHI" -> TBx (GT A.U) | "BLS" -> TBx (LE A.U)
      | "BGE" -> TBx (GE A.S) | "BLT" -> TBx (LT A.S)
      | "BGT" -> TBx (GT A.S) | "BLE" -> TBx (LE A.S)

      (* claude: case 8 (a distinct LTYPE from Shift's own case-8
       * mnemonic above -- goken reuses case numbers across
       * completely different grammar productions/optab rows, same as
       * every other arch ported so far). *)
      | "CBZ" -> TCBx false | "CBNZ" -> TCBx true

      (* claude: case 40 -- TBZ/TBNZ (test bit and branch). *)
      | "TBZ" -> TTBx false | "TBNZ" -> TTBx true

      (* claude: bare condition-code operand (goken's own LCOND lexer
       * class) -- a different token from TBx's branch mnemonics
       * (Parser_asm7.mly's `cond` rule), used by CSEL/CSET/CINC/CNEG/
       * CINV/CSINC/CSINV/CSNEG/CSETM below. Same aliasing convention
       * as BCS/BHS/BCC/BLO above. *)
      | "EQ" -> TCOND EQ | "NE" -> TCOND NE
      | "CS" -> TCOND (GE A.U) | "HS" -> TCOND (GE A.U)
      | "CC" -> TCOND (LT A.U) | "LO" -> TCOND (LT A.U)
      | "MI" -> TCOND MI | "PL" -> TCOND PL
      | "VS" -> TCOND VS | "VC" -> TCOND VC
      | "HI" -> TCOND (GT A.U) | "LS" -> TCOND (LE A.U)
      | "GE" -> TCOND (GE A.S) | "LT" -> TCOND (LT A.S)
      | "GT" -> TCOND (GT A.S) | "LE" -> TCOND (LE A.S)
      | "AL" -> TCOND AL

      (* claude: case 18 -- CSEL/CSINC/CSINV/CSNEG cond,Rn,[Rm,]Rd.
       * CINC/CINV/CNEG (and their *W forms) are goken's own 2-register
       * alias spellings for CSINC/CSINV/CSNEG respectively -- mapped
       * onto the *same* opcode constructor here, since the shared
       * base opcode and the 2-vs-3-register arity (not the mnemonic
       * name) is what actually decides the encoding -- see
       * CondSel's own AST comment. *)
      | "CSEL" -> TCONDSEL CSEL | "CSELW" -> TCONDSEL CSELW
      | "CSINC" -> TCONDSEL CSINC | "CSINCW" -> TCONDSEL CSINCW
      | "CINC" -> TCONDSEL CSINC | "CINCW" -> TCONDSEL CSINCW
      | "CSINV" -> TCONDSEL CSINV | "CSINVW" -> TCONDSEL CSINVW
      | "CINV" -> TCONDSEL CSINV | "CINVW" -> TCONDSEL CSINVW
      | "CSNEG" -> TCONDSEL CSNEG | "CSNEGW" -> TCONDSEL CSNEGW
      | "CNEG" -> TCONDSEL CSNEG | "CNEGW" -> TCONDSEL CSNEGW

      (* claude: case 18 -- CSET/CSETM cond,Rd. *)
      | "CSET" -> TCONDSET CSET | "CSETW" -> TCONDSET CSETW
      | "CSETM" -> TCONDSET CSETM | "CSETMW" -> TCONDSET CSETMW

      (* claude: case 58/59 -- the X-width (64-bit) exclusive-monitor
       * atomic pair, both plain and acquire/release flavors -- see
       * Ast_asm7.ml's LoadExcl/StoreExcl comment for what's
       * deliberately out of scope (sub-word forms, plain LDAR/STLR). *)
      | "LDXR" -> TLDXR false | "LDAXR" -> TLDXR true
      | "STXR" -> TSTXR false | "STLXR" -> TSTXR true

      | "SVC" -> TSVC
      | "RETURN" -> TRETURN

      (* claude: goken's own a.y lexes both "ZR" and "RSP" to the exact
       * same D_REG/reg=31 node (see Ast_asm7.ml's prelude comment) --
       * mirrored here by mapping both spellings to the same TRx (R 31)
       * token, letting Codegen7.ml's per-instruction encoding decide
       * whether register 31 in a given position means the zero
       * register or the stack pointer, exactly as real AArch64 does. *)
      | "ZR" -> TRx (A.R 31) | "RSP" -> TRx (A.R 31)

      | _ -> TIDENT s
      )

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

let parse (caps : < Cap.open_in; .. >) (conf : Preprocessor.conf)
      (file : Fpath.t) :
    Ast_asm7.program =
  let hooks = Parse_cpp.{
     lexer = token;
     parser = Parser_asm7.program;
     category = (fun t ->
       match t with
       | Parser_asm7.EOF -> Parse_cpp.Eof
       | Parser_asm7.TSharp -> Parse_cpp.Sharp
       | Parser_asm7.TIDENT s -> Parse_cpp.Ident s
       | _ -> Parse_cpp.Other
     );
     eof = Parser_asm7.EOF;
  }
  in
  Parse_cpp.parse caps hooks conf file

(* Simpler code path; possibly useful in tests *)
let parse_no_cpp (chan : Chan.i) : Ast_asm7.program =
  L.line := 1;
  let lexbuf = Lexing.from_channel chan.ic in
  try
    Parser_asm7.program token lexbuf, []
  with Parsing.Parse_error ->
      failwith (spf "Syntax error: line %d" !L.line)
