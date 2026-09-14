%{
(* Claude Code, Copyright (C) 2026 Yoann Padioleau, see copyright.txt *)
open Common
open Either

open Ast_asm
open Ast_asm6
open Parser_asm
module L = Location_cpp

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* The 6a amd64 assembly grammar -- see Ast_asm6.ml's own prelude for the
 * overall scope so far (64-bit arithmetic/compare/move/lea/call/
 * short-jumps/ret/syscall, R8-R15, no byte/word/long forms, no
 * floating point).
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
(* See Parser_asm.ml *)

(* claude: purely a parser-internal helper type -- see `move_operand`'s
 * own grammar comment for why MOVQ's grammar production needs a
 * 3-way union of `gen`/`ximm`/`xreg` rather than three separate
 * competing productions. *)
type move_operand_ =
  | MOGen of gen
  | MOImm of A.ximm
  | MOXreg of xregister

%}

/*(*************************************************************************)*/
/*(*1 Tokens *)*/
/*(*************************************************************************)*/

/*(*-----------------------------------------*)*/
/*(*2 opcodes *)*/
/*(*-----------------------------------------*)*/

%token <Ast_asm6.width * Ast_asm6.arith_opcode> TARITH
%token <Ast_asm6.width> TCMP
%token <Ast_asm6.width> TTEST
%token <Ast_asm6.width> TCMPXCHG
%token TLOCK
%token TPSLLQ
%token <Ast_asm6.width * Ast_asm6.shift_opcode> TSHIFT
%token <Ast_asm6.width> TMOV
%token <Ast_asm6.extend_opcode> TEXTEND
%token <Ast_asm6.width * Ast_asm6.unary_opcode> TUNARY
%token <Ast_asm6.width * Ast_asm6.muldiv_opcode> TMULDIV
/*(* claude: IMUL's mnemonic (e.g. "IMULQ") is shared between the
   * single-operand (`MulDiv`) and 2-operand (`Imul2`) grammar
   * productions below -- real 6a distinguishes purely by operand
   * *count*, not spelling, so this needs its own token (not folded
   * into TMULDIV, which MUL/DIV/IDIV never share a 2-operand form
   * with) so the grammar can look ahead past `gen` to decide. *)*/
%token <Ast_asm6.width> TIMUL
%token TCWD TCDQ TCQO
%token TLEA
%token TCALL
%token TJMP
%token <Ast_asm6.condition> TJcc
%token TRET
%token TSYSCALL
%token TNOP
/*(* claude: SSE only (single- and double-precision), no x87 -- see
   * Ast_asm6.ml's own "Scope so far" note. Each token carries an
   * `A.floatp_precision` (MOVSD/MOVSS etc share one grammar rule,
   * precision threaded through like every other AST case here). *)*/
%token <Ast_asm.floatp_precision> TMOVF
%token <Ast_asm6.arithf_opcode * Ast_asm.floatp_precision> TARITHF
%token <Ast_asm.floatp_precision> TUCOMISF
%token <Ast_asm6.width * Ast_asm.floatp_precision> TCVTINTTOF
%token <Ast_asm6.width * Ast_asm.floatp_precision> TCVTFTOINT
%token <Ast_asm.floatp_precision> TCVTFPREC
%token <Ast_asm.floatp_precision> TXORCLEARF

%token TTEXT TGLOBL
%token TDATA TWORD
/*(* claude: not wired in this arch's grammar (no virtual RET/NOP here
   * -- see Ast_asm6.ml's Ret; goken's real amd64 NOP is a genuine
   * instruction but not needed by hello_linux_amd64.s), kept only so
   * Parse_asm6.ml's T.TXXX passthrough stays exhaustive, same
   * shared-but-unused convention as TLBRACKET/TBANG below. *)*/
%token TEND

/*(*-----------------------------------------*)*/
/*(*2 registers *)*/
/*(*-----------------------------------------*)*/

%token <Ast_asm.register> TRx
%token TR
/*(* claude: shared-but-unused here -- see Ast_asm.ml/Token_asm.ml's own
   * comments for what these are for on other archs (this arch's own
   * XMM registers, below, are a completely separate token/type, *not*
   * this shared `fregister`). *)*/
%token <Ast_asm.fregister> TFx
%token TF
/*(* claude: XMM registers (X0-X15) -- goken's own lex.c lists these as
   * 16 individual named tokens ("X0".."X15"), not a generic "letter +
   * digit" rule the way R8-R15 get one in the shared Lexer_asm.mll, so
   * this port mirrors that with 16 explicit TIDENT cases in
   * Parse_asm6.ml (same convention as AX/CX/DX/BX/SI/DI's own named-
   * register cases there) rather than touching the shared lexer. *)*/
%token <Ast_asm6.xregister> TXx
%token TPC TSB TFP TSP

%token TC
/*(* claude: shared-but-unused here (see Token_asm.ml's own comments
   * for what these are for on other archs). *)*/
%token TLBRACKET TRBRACKET
%token TBANG

/*(*-----------------------------------------*)*/
/*(*2 Constants *)*/
/*(*-----------------------------------------*)*/

%token <int> TINT
%token <float> TFLOAT
%token <string> TSTRING

/*(*-----------------------------------------*)*/
/*(*2 Names *)*/
/*(*-----------------------------------------*)*/
%token <string> TIDENT

/*(*-----------------------------------------*)*/
/*(*2 Punctuation *)*/
/*(*-----------------------------------------*)*/

/*(* line number *)*/
%token <int> TSEMICOLON

%token TCOLON TDOT TDOLLAR
%token TOPAR TCPAR

/*(*-----------------------------------------*)*/
/*(*2 Operators *)*/
/*(*-----------------------------------------*)*/

%token TPLUS TMINUS TTILDE TMUL TMOD
%token TSLASH
%token TLT TGT
/*(* claude: bitwise and/or/xor in constant expressions -- %left alone
   * does NOT declare a token for this ocamlyacc (see Parser_asm5.mly's
   * identical TLT/TGT comment); every other arch's grammar has this
   * same latent, never-triggered gap (no fixture uses bitwise ops in a
   * constant expression) since `expr`'s TAND/TOR/TXOR rules were
   * copied forward without ever adding the matching %token. *)*/
%token TAND TOR TXOR

/*(*-----------------------------------------*)*/
/*(*2 Misc *)*/
/*(*-----------------------------------------*)*/
%token TSharp
%token EOF

/*(*************************************************************************)*/
/*(*1 Priorities *)*/
/*(*************************************************************************)*/
%left TOR
%left TXOR
%left TAND
%left TLT TGT
%left TPLUS TMINUS
%left TMUL TSLASH TMOD

/*(*************************************************************************)*/
/*(*1 Rules type declaration *)*/
/*(*************************************************************************)*/

%type <Ast_asm6.instr Ast_asm.lines> program
%start program

%%

/*(*************************************************************************)*/
/*(*1 Program (arch independent, same as Parser_asm5.mly/Parser_asm7.mly) *)*/
/*(*************************************************************************)*/

program: lines EOF { $1 }

lines:
 | /*empty*/  { [] }
 | line lines { $1 @ $2 }

line:
 |               TSEMICOLON { [] }
 | instr         TSEMICOLON { [(Instr $1, $2)] }
 | pseudo_instr  TSEMICOLON { [(Pseudo $1, $2)] }
 /*(* claude: end-of-file marker (real 6a accepts a bare "END", no
    * operands) -- a true no-op, same as every other arch's own
    * "TEND TSEMICOLON" line rule (ARM32/ARM64/MIPS's own
    * Parser_asm{5,7,v}.mly) -- just never wired here before, since
    * no amd64_diff fixture happened to end with a real "END" line
    * until this closure stress test hit one (real 6c -S always
    * emits a trailing "END" after the comma-padding fix strips its
    * own dangling comma). *)*/
 | TEND          TSEMICOLON { [] }

 | label_def line           { $1::$2 }

label_def: TIDENT TCOLON    { (LabelDef $1, !L.line) }

/*(*************************************************************************)*/
/*(*1 Pseudo instructions (arch independent) *)*/
/*(*************************************************************************)*/
pseudo_instr:
 | TTEXT  global TC imm
     { TEXT  ($2, default_attr, $4) }
 | TGLOBL global TC imm
     { GLOBL ($2, default_attr, $4) }

 | TTEXT global TC con TC imm
     { TEXT ($2, attributes_of_int $4, $6) }
 | TGLOBL global TC con TC imm
     { GLOBL ($2, attributes_of_int $4, $6) }

 | TDATA global_and_offset TSLASH con TC ximm
     { DATA (fst $2, snd $2, $4, $6) }
 | TWORD ximm
     { WORD $2 }

global: name
  { match $1 with
    | Global (e, 0) -> e
    | _ -> error "global (without any offset) expected"
  }

global_and_offset: name
  { match $1 with
    | Global (e, n) -> (e, n)
    | _ -> error "global with offset expected"
  }

/*(*************************************************************************)*/
/*(*1 Instructions *)*/
/*(*************************************************************************)*/
instr:
 /*(* goken's yaddl/yxorl-shaped 2-operand arithmetic: "ADDQ $imm,Rd" /
    * "ADDQ Rs,Rd" (or ADDL/etc for the 32-bit form) -- see
    * Ast_asm6.ml's Arith comment. *)*/
 | TARITH imr TC gen             { let (w, op) = $1 in Arith (w, op, $2, $4) }

 /*(* goken's ycmpl-shaped compare: "CMPQ gen,imr" -- see Ast_asm6.ml's
    * Cmp comment for the reversed-from-Arith operand-role order. *)*/
 | TCMP gen TC imr               { Cmp ($1, $2, $4) }

 /*(* goken's ytestl/ytestb-shaped TEST: "TESTQ Rs,gen" -- see
    * Ast_asm6.ml's Test comment for the register-first role order
    * (a third, distinct convention from both Arith's and Cmp's own). *)*/
 | TTEST reg TC gen              { Test ($1, $2, $4) }

 /*(* goken's yrl_ml/yrb_mb-shaped CMPXCHG: "CMPXCHGQ Rs,gen" -- see
    * Ast_asm6.ml's CmpXchg comment. *)*/
 | TCMPXCHG reg TC gen           { CmpXchg ($1, $2, $4) }
 | TLOCK                         { Lock }

 /*(* goken's yshl/yshb-shaped shift: "SHLQ $imm,gen" / "SHLQ Rs,gen"
    * -- reuses `imr` for the amount at the grammar level (no real
    * ambiguity to resolve there), converted to the dedicated
    * `shift_amount` type here since a *general* register isn't valid
    * -- see Ast_asm6.ml's Shift comment. *)*/
 | TSHIFT imr TC gen             { let (w, op) = $1 in
                                    let amount = match $2 with
                                      | Imm v -> ShiftImm v
                                      | Reg r -> ShiftReg r
                                      | Mem _ -> error "shift amount can't be a memory operand"
                                      | Addr _ -> error "shift amount can't be an address immediate"
                                    in
                                    Shift (w, op, amount, $4) }

 /*(* goken's ymovq/ymovl-shaped move: covers register/memory/immediate
    * in every combination MOVQ/MOVL actually need, *and* (MOVQ only)
    * the raw GP<->XMM bit-copy shape (see Ast_asm6.ml's Move/
    * MovQToXmm/MovQFromXmm comments) -- left-factored into one shared
    * "TMOV move_operand TC move_operand" production (disambiguated by
    * a semantic-action match, not by competing grammar productions)
    * since `gen` and `lgen` sharing a prefix with a *third* operand
    * kind (`xreg`) at the grammar level is a genuine LALR(1) shift/
    * reduce conflict, confirmed the hard way: ocamlyacc silently
    * picked one interpretation, breaking every ordinary "MOVQ
    * Rs,Rd"/"MOVQ $imm,Rd" fixture until this was left-factored. *)*/
 | TMOV move_operand TC move_operand {
     match $1, $2, $4 with
     | w, MOGen g1, MOGen g2 -> Move (w, Either.Left g1, g2)
     | w, MOImm i1, MOGen g2 -> Move (w, Either.Right i1, g2)
     | Q_, MOGen g1, MOXreg x2 -> MovQToXmm (g1, x2)
     | Q_, MOXreg x1, MOGen g2 -> MovQFromXmm (x1, g2)
     | _ -> error "invalid MOV operand combination (raw GP<->XMM MOV is only wired for MOVQ)"
   }

 /*(* goken's yps-shaped PSLLQ (shift-by-immediate only) -- see
    * Ast_asm6.ml's PsllQXmm comment. *)*/
 | TPSLLQ imm TC xreg            { PsllQXmm ($4, $2) }

 /*(* goken's ymb_rl/yml_rl-shaped sign/zero-extending move -- see
    * Ast_asm6.ml's Extend comment ("MOVLQZX" is deliberately absent
    * here, mapped onto TMOV L_ instead in Parse_asm6.ml). *)*/
 | TEXTEND gen TC reg            { Extend ($1, $2, $4) }

 /*(* goken's yincb/yincl/yincw/yscond-shaped single-operand ModRM-
    * extension-group op: "NEGQ gen" / "INCQ gen" -- see Ast_asm6.ml's
    * Unary comment. *)*/
 | TUNARY gen                    { let (w, op) = $1 in Unary (w, op, $2) }

 /*(* goken's ydivl/ydivb-shaped single-operand MUL/DIV/IDIV -- see
    * Ast_asm6.ml's MulDiv comment. *)*/
 | TMULDIV gen                   { let (w, op) = $1 in MulDiv (w, op, $2) }
 /*(* IMUL's own single-operand (goken's yimul row0, same MulDiv shape
    * as MUL/DIV/IDIV) vs 2-operand ("IMULQ gen,Rd", goken's yimul
    * row3, Imul2) forms -- disambiguated by operand count, not
    * spelling, hence the shared TIMUL token (see its own comment
    * above). *)*/
 | TIMUL gen                     { MulDiv ($1, IMUL_, $2) }
 | TIMUL gen TC reg              { Imul2 ($1, $2, $4) }

 | TCWD                          { Cwd }
 | TCDQ                          { Cdq }
 | TCQO                          { Cqo }

 /*(* goken's real LEA -- any memory operand (register-indirect, scaled-
    * index, global, or local), see Ast_asm6.ml's Lea comment. *)*/
 | TLEA gen TC reg { Lea ($2, $4) }

 /*(* direct near call/jump, goken's ycall/yjmp's 0xe8/0xe9 rel32 forms
    * (the indirect-through-register/memory forms aren't wired). *)*/
 | TCALL branch                  { Call $2 }
 | TJMP branch                   { Jmp ($2, ref false) }
 /*(* goken's yjcond-shaped conditional jump -- always to a label
    * (goken's own Ybr class), never register-indirect. *)*/
 | TJcc rel                      { Jcc ($1, $2, ref false) }

 | TRET                          { Ret }
 | TSYSCALL                      { Syscall }

 /*(* goken's yxmov-shaped MOVSD/MOVSS -- see Ast_asm6.ml's MovF comment
    * for why its codegen clause order is opposite from TMOV's own. *)*/
 | TMOVF xgen TC xgen            { MovF ($1, $2, $4) }
 /*(* goken's yxm-shaped dyadic SSE arithmetic: "ADDSD/ADDSS Xm/mem,Xn"
    * (in-place, "Xn += Xm/mem") -- see Ast_asm6.ml's ArithF comment. *)*/
 | TARITHF xgen TC xreg          { let (op, prec) = $1 in ArithF (op, prec, $2, $4) }
 /*(* goken's yxcmp-shaped UCOMISD/UCOMISS -- see Ast_asm6.ml's CmpF
    * comment. *)*/
 | TUCOMISF xgen TC xreg         { CmpF ($1, $2, $4) }
 /*(* goken's yxcvlf/yxcvqf-shaped CVTSQ2SD/CVTSQ2SS (int64 -> float)
    * and yxcvfq-shaped CVTTSD2SQ/CVTTSS2SQ (float -> int64, truncating)
    * -- see Ast_asm6.ml's CvtIntToF/CvtFToInt comments. *)*/
 | TCVTINTTOF gen TC xreg        { let (w, prec) = $1 in CvtIntToF (w, prec, $2, $4) }
 | TCVTFTOINT xgen TC reg        { let (w, prec) = $1 in CvtFToInt (w, prec, $2, $4) }
 /*(* goken's yxm-shaped CVTSD2SS/CVTSS2SD (float precision conversion,
    * one real opcode both directions, disambiguated by prefix) -- see
    * Ast_asm6.ml's CvtFPrec comment. *)*/
 | TCVTFPREC xgen TC xreg        { CvtFPrec ($1, $2, $4) }
 /*(* goken's yxm-shaped XORPD/XORPS -- only the self-clear "Xn,Xn"
    * idiom is wired, see Ast_asm6.ml's XorClearF comment. *)*/
 | TXORCLEARF xreg TC xreg       { if $2 = $4 then XorClearF ($1, $2)
                                    else error "XORPD/XORPS only wired for the self-clear Xn,Xn idiom" }

/*(*************************************************************************)*/
/*(*1 Operands *)*/
/*(*************************************************************************)*/

imr:
 | imm                 { Imm $1 }
 | reg                 { Reg $1 }
 | ireg                { Mem (Indirect ($1, 0)) }
 | con ireg            { Mem (Indirect ($2, $1)) }
 | name                { Mem (Entity $1) }
 /*(* claude: "s+0(FP)" as an Arith *source* -- e.g. real "SUBQ
    * s+0(FP),AX" (port_strlen.c.s's real strlen) -- same named-local-
    * against-a-real-register case as `gen`'s own analogous alternative,
    * just for `imr`'s own memory case (see Ast_asm6.ml's `imr` comment
    * for why real ADD/SUB/etc. accept a memory source at all). *)*/
 | TIDENT offset TOPAR TSP TCPAR { ignore $1; Mem (LocalSP $2) }
 /*(* claude: "$fmtalloc<>+1032(SB)" -- an address-of-global immediate,
    * see Ast_asm6.ml's `imr`/Addr comment (real fmt/fmt.c's own "CMPQ
    * DX,$fmtalloc<>+1032(SB)"). *)*/
 | TDOLLAR name        { Addr $2 }
 /*(* claude: same scaled-index case as `gen`'s own analogous
    * alternatives above -- real fmt/strtod.c's own "ADDL
    * low+-40(SP)(CX*4),AX". *)*/
 | ireg scaled_index                          { Mem (IndirectScaled ($1, 0, fst $2, snd $2)) }
 | con ireg scaled_index                      { Mem (IndirectScaled ($2, $1, fst $3, snd $3)) }
 | name scaled_index                          { Mem (EntityScaled ($1, fst $2, snd $2)) }
 | TIDENT offset TOPAR TSP TCPAR scaled_index { ignore $1; Mem (LocalSPScaled ($2, fst $6, snd $6)) }

imm: TDOLLAR con      { $2 }

/*(* claude: named low registers (AX/CX/DX/BX/SI/DI) are recognized as
   * plain TIDENT by the shared lexer and mapped to a register value in
   * Parse_asm6.ml, same convention as every arch's own mnemonic-string
   * dispatch -- there is no generic "name a register by string" rule in
   * Lexer_asm.mll the way there is for "R" + digit. SP is its own
   * shared token (TSP) -- see Ast_asm6.ml's prelude for why it's a
   * plain `reg` here (a real register on this arch) and *not* routed
   * through `pointer`'s virtual-addressing mechanism below, unlike
   * every other arch. R8-R15 need no grammar case at all -- the shared
   * "R"+digit lexer rule already produces them as plain TRx tokens,
   * matched by the very next alternative below (Codegen6.ml's `rex`
   * threads the needed REX.R/.B bits through). *)*/
reg:
 | TRx                { $1 }
 | TSP                { rSP }
 | TR TOPAR expr TCPAR
     { if $3 <= 15 && $3 >= 0
       then R $3
       else error "register value out of range"
     }

/*(* claude: "0(SP)"/"8(SP)" -- a real, concrete indirect-with-
   * displacement addressing mode on this arch (unlike every other
   * arch's own `gen`, this doesn't need a *virtual* SP-relative case at
   * all, precisely because "SP" already resolves to a real `reg` above
   * rather than going through `pointer`). `name` covers the *symbolic*
   * memory-operand cases instead (SB-relative globals as a plain memory
   * reference i.e. without a leading "$", and FP-relative parameters,
   * e.g. "buf+0(FP)" as MOVQ's *source*). *)*/
/*(* claude: real x86 SIB scaled-index "(CX*4)" suffix -- confirmed
   * against goken's real assemblers/6a/a.y (`checkscale`): only 1/2/4/8
   * are valid multipliers, everything else is a real assembler error,
   * not a codegen-time Impossible/Todo. *)*/
scaled_index:
 | TOPAR reg TMUL con TCPAR
     { if $4 = 1 || $4 = 2 || $4 = 4 || $4 = 8
       then ($2, $4)
       else error "scale must be 1, 2, 4, or 8"
     }

gen:
 | reg                 { GReg $1 }
 | ireg                { Indirect ($1, 0) }
 | con ireg            { Indirect ($2, $1) }
 | name                { Entity $1 }
 /*(* claude: "u+8(SP)"/"u-8(SP)" -- a real, *named* local-variable
    * reference against the real SP register (confirmed against real
    * 6a: it accepts this and it's genuinely common, real 6c -S
    * output labels every stack slot with the C variable's own name
    * even though SP is a real, concrete register here, unlike every
    * other arch's own virtual FP/SP addressing -- see `gen`'s own
    * comment above and Ast_asm6.ml's prelude for why TSP never goes
    * through `pointer`/`name`/Entity at all). The leading identifier
    * is purely cosmetic for this port's own purposes (no local-
    * variable-name tracking to validate it against) -- discarded. Real
    * 6a/6l does NOT treat this like the bare, unlabeled "8(SP)" form
    * just above (confirmed the hard way against real fmt/vfprint.c's
    * own "f+-104(SP)"/"buf+-360(SP)": with vfprint's own real $400
    * frame these assemble to "lea 0x128(%rsp)"/"lea 0x28(%rsp)", i.e.
    * hardware offset autosize+N, not the raw N this port's earlier,
    * unverified version used) -- see Ast_asm6.ml's own LocalSP
    * comment for the full story. *)*/
 | TIDENT offset TOPAR TSP TCPAR { ignore $1; LocalSP $2 }
 /*(* claude: real SIB scaled-index addressing -- "(BX)(CX*4)" (bare
    * register base), "8(BX)(CX*4)" (register base with an offset), and
    * "tab<>+0(SB)(CX*8)" (SB-relative global base, no base register at
    * all) -- see `gen`'s own scaled_index comment and Ast_asm6.ml's
    * IndirectScaled/EntityScaled comment. Found stress-testing real
    * lib_core/libc (fmt/dofmt.c's own "LEAQ (BX)(CX*1),AX" and
    * fmt/strtod.c's own "_ctype+0(SB)(CX*1)"). *)*/
 | ireg scaled_index                          { IndirectScaled ($1, 0, fst $2, snd $2) }
 | con ireg scaled_index                      { IndirectScaled ($2, $1, fst $3, snd $3) }
 | name scaled_index                          { EntityScaled ($1, fst $2, snd $2) }
 /*(* claude: same named-local-against-real-SP-register case as this
    * `gen`'s own TIDENT-offset alternative above, plus a scaled index --
    * real fmt/strtod.c's own "a+-1573(SP)(CX*1)". *)*/
 | TIDENT offset TOPAR TSP TCPAR scaled_index { ignore $1; LocalSPScaled ($2, fst $6, snd $6) }

/*(* claude: XMM register-or-memory operand -- same addressing modes as
   * `gen` above (memory is still addressed through an ordinary GP
   * `reg`, e.g. "-8(SP)"; only the *register* alternative differs) --
   * see Ast_asm6.ml's `xgen` comment for why this isn't just `gen`
   * with an extra case. *)*/
xreg:
 | TXx { $1 }

xgen:
 | xreg                { XReg $1 }
 | ireg                { XIndirect ($1, 0) }
 | con ireg            { XIndirect ($2, $1) }
 | name                { XEntity $1 }
 /*(* claude: same "named local against the real SP register" case as
    * `gen`'s own analogous alternative above (e.g. real MOVSD's own
    * "u+-8(SP)" memory operand) -- xreg's memory operand shares the
    * exact same real addressing modes as `gen`'s, this is just the
    * XMM/float-instruction-operand sibling (see Ast_asm6.ml's LocalSP
    * comment for why this needs env.autosize, not the raw offset). *)*/
 | TIDENT offset TOPAR TSP TCPAR { ignore $1; XLocalSP $2 }
 /*(* claude: same scaled-index case as `gen`'s own analogous
    * alternatives above (e.g. real fmt/fltfmt.c's own "MOVSD
    * pows10<>+0(SB)(AX*8),X0"). *)*/
 | ireg scaled_index                          { XIndirectScaled ($1, 0, fst $2, snd $2) }
 | con ireg scaled_index                      { XIndirectScaled ($2, $1, fst $3, snd $3) }
 | name scaled_index                          { XEntityScaled ($1, fst $2, snd $2) }
 | TIDENT offset TOPAR TSP TCPAR scaled_index { ignore $1; XLocalSPScaled ($2, fst $6, snd $6) }
 /*(* claude: "$(1.0e+00)" -- a literal float source, see Ast_asm6.ml's
    * XFloatImm comment (real fmt/fltfmt.c's own "MOVSD $(1.0e+00),X0"
    * and "MULSD $(3.0e-01),X0"). *)*/
 | fcon                                        { XFloatImm $1 }

ximm:
 | imm             { Int $1 }
 | fcon            { Float $1 }
 | TDOLLAR TSTRING { String $2 }
 | TDOLLAR name    { Address $2 }

/*(* claude: `move_operand` -- the left-factored union of `gen`/`ximm`/
   * `xreg` used by MOVQ's own shared grammar production above (see
   * that production's own comment for why this couldn't stay three
   * separate rules). *)*/
move_operand:
 | gen  { MOGen $1 }
 | ximm { MOImm $1 }
 | xreg { MOXreg $1 }

ireg: TOPAR reg TCPAR { $2 }

branch:
 | rel               { $1 }
 | global             { ref (SymbolJump $1) }
 | ireg              { ref (IndirectJump $1) }
 /*(* claude: real 6a's own indirect CALL/JMP takes a *bare* register,
    * no parens (confirmed against real 6a: "CALL BX"/"JMP BX") --
    * unlike ARM64's own "(R1)" convention this file's `ireg`
    * alternative above was copied from. Both are accepted here (`reg`
    * alone is unambiguous with `rel`'s own TIDENT-based start, so no
    * conflict), but only the bare form is real amd64 syntax. *)*/
 | reg               { ref (IndirectJump $1) }

rel:
 | TIDENT offset        { ref (LabelUse ($1, $2)) }
 | con TOPAR TPC TCPAR  { ref (Relative $1) }

/*(*-----------------------------------------*)*/
/*(*2 name and offset (arch independent, but this arch's own `pointer`
   * only wires TSB/TFP -- see this file's `gen`/`reg` comments for why
   * TSP is deliberately absent here) *)*/
/*(*-----------------------------------------*)*/

name:
 | TIDENT offset         TOPAR pointer TCPAR { $4 (Some (mk_g $1 false)) $2 }
 | TIDENT TLT TGT offset TOPAR TSB     TCPAR { Global (mk_g $1 true, $4) }

pointer:
 | TSB  { (fun name_opt offset ->
           match name_opt with
           | None -> error "identifier expected"
           | Some e -> Global (e, offset)
          )
         }
 /*(* claude: TFP maps to the shared `Local` constructor, matching
    * ARM32/ARM64's own established (if confusingly-named) convention
    * -- see Ast_asm6.ml's prelude and Codegen6.ml's own resolution
    * comment for the real, amd64-specific offset bias (return-address
    * slot, +8) this eventually gets resolved to. *)*/
 | TFP  { (fun name_opt offset ->
           match name_opt with
           | None -> Local (None, offset)
           | Some ({name = s; priv = _false; signature = _}) -> Local (Some s, offset)
           )
         }

offset:
 | /* empty */ { 0 }
 | TPLUS  con  { $2 }
 | TMINUS con  { - $2 }

/*(*-----------------------------------------*)*/
/*(*2 float (arch independent; not wired for real use yet) *)*/
/*(*-----------------------------------------*)*/

fcon:
 | TDOLLAR TFLOAT               { $2 }
 | TDOLLAR TMINUS TFLOAT        { -. $3 }
 /*(* claude: "$(1.0e+00)" -- confirmed against goken's real
    * assemblers/6a/a.y ("'$' '(' LFCONST ')'"), genuinely common in
    * real fmt/fltfmt.c's own DATA statements (a parenthesized float
    * constant table, one entry per line). *)*/
 | TDOLLAR TOPAR TFLOAT TCPAR   { $3 }

/*(*-----------------------------------------*)*/
/*(*2 number constant and expression (arch independent)  *)*/
/*(*-----------------------------------------*)*/

con:
 | TINT { $1 }

 | TMINUS con { - $2 }
 | TPLUS  con { $2 }
 | TTILDE con { lnot $2 }

 | TOPAR expr TCPAR { $2 }

expr:
 | con { $1 }

 | expr TPLUS expr  { $1 + $3 }
 | expr TMINUS expr { $1 - $3 }
 | expr TMUL expr   { $1 * $3 }
 | expr TSLASH expr { $1 / $3 }
 | expr TMOD expr   { $1 mod $3 }

 | expr TLT TLT expr { $1 lsl $4 }
 | expr TGT TGT expr { $1 asr $4 }

 | expr TAND expr    { $1 land $3 }
 | expr TOR expr     { $1 lor $3 }
 | expr TXOR expr    { $1 lxor $3 }
