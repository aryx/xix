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
  | T.TEND -> TEND
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
  | T.TLBRACKET-> TLBRACKET
  | T.TRBRACKET-> TRBRACKET
  | T.TBANG-> TBANG
  | T.TLT-> TLT
  | T.TGT-> TGT
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
       * done -- see docs/claude_notes/riscv_port.md.
       *)

      (* MOVW $imm,R / MOVW $sym(SB),R / MOVW R,R / MOVW mem,R / MOVW R,mem
       * all go through Move2's W__ (word) case, the same as ARM/MIPS's
       * unified Move2 -- unlike MOVB/MOVH, which need Move1 for their
       * sign/zero-extension distinction on loads (not wired yet).
       *)
      | "MOVW" -> TMOVE2 W__
      (* claude: goken's real "MOV" (its own AMOV, a distinct opcode
       * from AMOVW's own lex.c entry) -- tagged onto its own `V__`
       * (bare/pointer-width), NOT aliased onto W__ the way MOVWU is
       * below. An earlier version of this comment claimed "MOV" and
       * "MOVW" were confirmed byte-identical for BOTH register-
       * immediate and register-register forms and aliased "MOV"
       * straight onto `W__`'s own token -- true for the immediate
       * case ("MOV $37,R10" does produce the same "addi"/"lui,addi"
       * bytes as "MOVW $37,R10", see Codegeni.ml's own V__
       * Int-immediate case, deliberately kept identical to W__'s),
       * but WRONG for the register-to-register case: hand-decoding
       * real goken bytes for "MOV R9,R10" vs "MOVW R9,R10" shows the
       * source register lands in a DIFFERENT operand slot of the
       * real "ADD rd,rs,x0" idiom each mnemonic expands to (MOVW:
       * rs1=source,rs2=x0; MOV: rs1=x0,rs2=source -- see Codegeni.ml's
       * own comment on its two separate reg-reg Move2 arms). Aliasing
       * "MOV" onto the same `W__` tag as "MOVW" made that distinction
       * inexpressible at the AST level, corrupting real "MOV
       * Rs,Rd" bytes once the closure exercised it (fmt/strtod.c's
       * own real "MOVW R0,R8" -- an artifact of the SAME real source
       * file that also has plain register moves). NOT verified for a
       * *memory* operand: goken's own optab.c shows AMOV's memory
       * forms use "sd"/"ld" (64-bit store/load) where AMOVW uses
       * "sw"/"lw" -- genuinely different real instructions -- so
       * "MOV Rd,off(Rb)" would need its own dedicated verification if
       * a real closure ever exercises it; none has so far. *)
      | "MOV" -> TMOVE2 V__
      (* claude: goken's real "MOVWU" (its own AMOVWU, a genuinely
       * separate opcode from AMOVW -- unsigned/zero-extending word
       * load, distinct from AMOVW's sign-extending one on a 64-bit
       * target). On this port's own RV32 target specifically, a
       * "word" already fills the entire register -- there's no bits
       * left above it to sign- or zero-extend into at all -- so
       * confirmed byte-identical against real ia/il: "MOVWU
       * a+0(FP),R9" produces the exact same bytes as "MOVW
       * a+0(FP),R9". Aliased directly onto `W__` for that reason.
       * Revisit if this port ever targets RV64 (arch "j") for real --
       * the distinction becomes meaningful there. Found stress-testing
       * real lib_core/libc (port/vlrt.c's own 64-bit-arithmetic
       * helpers, which load each 32-bit half of a real int64 with
       * explicit sign-vs-zero-extend intent even though it's a no-op
       * on this 32-bit target). *)
      | "MOVWU" -> TMOVE2 W__

      (* claude: byte/half loads/stores and register-to-register
       * sign/zero extend (case 6/7/10) -- Ast_asmi's move1_size
       * bundles size+sign together (B_/H_ of A.sign), unlike goken's
       * grammar which has four separate mnemonics. *)
      | "MOVB" -> TMOVE1 (B_ A.S) | "MOVBU" -> TMOVE1 (B_ A.U)
      | "MOVH" -> TMOVE1 (H_ A.S) | "MOVHU" -> TMOVE1 (H_ A.U)

      (* claude: case 17 -- fcvt, split by register-file direction
       * (see Ast_asmi.ml's FCVTFF/FCVTFI/FCVTIF comment). The
       * unsigned-source int->float/double forms ARE real goken
       * mnemonics ("MOVUF"/"MOVUD", confirmed in lex.c/optab.c) --
       * but the reverse direction (float->unsigned-int) has no
       * mnemonic in goken's own grammar (no MOVFWU/MOVDWU), so
       * FCVTFI stays signed-only. *)
      | "MOVFD" -> TFCVTFF MOVFD | "MOVDF" -> TFCVTFF MOVDF
      | "MOVFW" -> TFCVTFI MOVFW | "MOVDW" -> TFCVTFI MOVDW
      | "MOVWF" -> TFCVTIF MOVWF | "MOVWD" -> TFCVTIF MOVWD
      | "MOVUF" -> TFCVTIF MOVUF | "MOVUD" -> TFCVTIF MOVUD

      (* claude: goken's own lex.c maps "MOVF"/"MOVD" to one LMOVF
       * token class covering register move (AMOVF/AMOVD -- real
       * RISC-V's own "FSGNJ.S/D Fd,Fs,Fs" self-sign-inject idiom, no
       * dedicated float-move opcode exists), memory access, and
       * float-immediate load all at once -- this port mirrors that by
       * routing all of them through the SAME generic TMOVE2 (F__/D__)
       * path (see Ast_asmi.ml's `gen`/`vgen`'s own GFReg comment),
       * rather than a separate per-shape construct. Found
       * stress-testing real lib_core/libc (fmt/strtod.c's own real
       * "MOVD F28,F0" register
       * move, "MOVD x+4(FP),F1" memory load, and "MOVD
       * pows10<>+1272(SB),F2" global load, all the SAME mnemonic). *)
      | "MOVF" -> TMOVE2 F__ | "MOVD" -> TMOVE2 D__

      (* claude: real float/double arithmetic (goken's own AADDF/
       * AADDD/etc, always the 3-explicit-register "Fa,Fb,Fc" form in
       * every real closure stress-tested so far -- see Parser_asmi.mly's
       * own `TARITHF freg TC freg TC freg` production, already wired,
       * just never reachable without these lexer entries). DIV_ and
       * ABS_/NEG_ aren't mapped here -- no real closure needs them
       * yet, and (per `arithf_opcode`'s own comment) ABS_/NEG_ don't
       * fit this 2-or-3-register shape at all. Found stress-testing
       * real lib_core/libc (fmt/strtod.c's own real string-to-double
       * parser, "ADDD"/"SUBD"/"MULD"). *)
      | "ADDF" -> TARITHF (ADD_, A.F) | "ADDD" -> TARITHF (ADD_, A.D)
      | "SUBF" -> TARITHF (SUB_, A.F) | "SUBD" -> TARITHF (SUB_, A.D)
      | "MULF" -> TARITHF (MUL_, A.F) | "MULD" -> TARITHF (MUL_, A.D)
      | "DIVF" -> TARITHF (DIV_, A.F) | "DIVD" -> TARITHF (DIV_, A.D)

      (* claude: floating-point compare -- see Ast_asmi.ml's CmpF
       * comment for why this is its own instruction, not ArithF. Only
       * EQ/LT/LE are wired (see that same comment). Found
       * stress-testing real lib_core/libc (fmt/fltfmt.c's own real
       * "CMPLTD"/"CMPLED"). *)
      | "CMPEQF" -> TCMPF (EQ_, A.F) | "CMPEQD" -> TCMPF (EQ_, A.D)
      | "CMPLTF" -> TCMPF (LT_, A.F) | "CMPLTD" -> TCMPF (LT_, A.D)
      | "CMPLEF" -> TCMPF (LE_, A.F) | "CMPLED" -> TCMPF (LE_, A.D)

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

      | "BEQ" -> TB EQ | "BNE" -> TB NE
      | "BLT" -> TB (LT A.S) | "BGE" -> TB (GE A.S)
      | "BLTU" -> TB (LT A.U) | "BGEU" -> TB (GE A.U)
      (* claude: LE/GT have no direct RISC-V hardware branch (only
       * BEQ/BNE/BLT/BGE/BLTU/BGEU exist) -- but goken's own assembler
       * DOES accept "BLE"/"BGT"/"BLEU"/"BGTU" as real mnemonics
       * (confirmed: `assemblers/ia/lex.c` maps all four to real
       * tokens), with the reversed-relation rewrite to LT/GE done by
       * the *linker*, not the assembler (`linkers/il/obj.c`'s own
       * case ABGT/ABGTU/ABLE/ABLEU) -- mirrored in this port's own
       * `Codegeni.ml` Bxx case, not here. Found stress-testing real
       * lib_core/libc (port/vlrt.c's/fmt/dofmt.c's own real "BLEU"
       * from a compiled unsigned-comparison). *)
      | "BLE" -> TB (LE A.S) | "BGT" -> TB (GT A.S)
      | "BLEU" -> TB (LE A.U) | "BGTU" -> TB (GT A.U)

      (* claude: goken's own lex.c maps both "JAL" and "JALR" to the
       * exact same AJAL token, dispatching on operand shape alone
       * (see Ast_asmi.ml's JALRI comment) -- mirrored here by mapping
       * both spellings to the same TJAL token. *)
      | "JMP" -> TJMP | "JAL" -> TJAL | "JALR" -> TJAL

      (* claude: standalone case 8 -- goken's grammar shape is
       * `LUI $I,D` / `LUI name,D` (outcode's `from`=imm/name,
       * `to`=D); only the immediate form is wired here (`imm`, not
       * `name`), matching what Codegeni.ml's case 8 implements. *)
      | "LUI" -> TLUI

      (* claude: case 22, control-and-status register access. "CSR"
       * itself is a separate keyword from the CSRRW/CSRRS/CSRRC
       * mnemonics -- mirrors goken's own lexer (LCTL vs LCSR),
       * "CSRRW CSR($num),S,D" being the concrete syntax. Immediate
       * variants (CSRRWI/CSRRSI/CSRRCI) aren't wired -- see
       * Ast_asmi.ml's CSR comment. *)
      | "CSR" -> TCSRTOK
      | "CSRRW" -> TCSR CSRRW
      | "CSRRS" -> TCSR CSRRS
      | "CSRRC" -> TCSR CSRRC

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
