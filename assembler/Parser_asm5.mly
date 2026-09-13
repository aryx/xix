/*(*s: Parser_asm5.mly *)*/
%{
(* Copyright 2015, 2016, 2025 Yoann Padioleau, see copyright.txt *)
open Common
open Either

open Ast_asm
open Parser_asm
open Ast_asm5
module L = Location_cpp

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* The 5a ARM assembly grammar.
 *
 * Limitations compared to 5a:
 *  - just imm for SWI
 *
 * todo:
 *  - special bits
 *  - lots of advanced instructions (float, mulm, ...)
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
(* See Parser_asm.ml *)

%}

/*(*************************************************************************)*/
/*(*1 Tokens *)*/
/*(*************************************************************************)*/

/*(*-----------------------------------------*)*/
/*(*2 opcodes *)*/
/*(*-----------------------------------------*)*/

%token <Ast_asm5.arith_opcode> TARITH
%token <Ast_asm5.arithf_opcode * Ast_asm.floatp_precision> TARITHF
%token <Ast_asm.floatp_precision> TCMPF
%token <Ast_asm.floatp_precision> TMOVWF TMOVFW
%token <Ast_asm.floatp_precision> TMOVF
%token <Ast_asm5.fcrreg> TFCR
%token <Ast_asm5.psrreg> TPSR
%token <Ast_asm.sign * bool> TMULL
%token TMVN
%token <Ast_asm.move_size> TMOV TSWAP
%token TB  TBL
%token TCASE TBCASE
%token <Ast_asm5.cmp_opcode> TCMP   
%token <Ast_asm5.condition> TBx TCOND
%token TSWI TRFE
%token TMOVM
/*(* MCR (0) / MRC (1) -- matches goken's own LSYSTEM token value,
   * used directly in the encoded word (see the pseudo_instr
   * production below). *)*/
%token <int> TMCR
/*(* case 38/39: the generic dot-suffix-flag token (P/U/W/S/F bits,
   * see Ast_asm5.ml's sflag_* comment) -- only MOVM's `condf` rule
   * (below) folds these in for now; every other instruction still
   * uses the plain `cond` rule unchanged, so this can't silently
   * change behavior of anything already working. *)*/
%token <int> TSUF
%token TLBRACKET TRBRACKET
%token TBANG
/*(* claude: "foo<>" static/local-symbol suffix, and "<<"/">>" shifts
   * (see the "%left TLT TGT" priority line below, and Token_asm.ml's
   * TLT/TGT comment) -- %left alone does NOT declare a token for this
   * ocamlyacc, unlike real yacc's convention. *)*/
%token TLT TGT

%token TRET TNOP TEND

%token TTEXT TGLOBL 
%token TDATA TWORD 

/*(*-----------------------------------------*)*/
/*(*2 registers *)*/
/*(*-----------------------------------------*)*/

%token <Ast_asm.register> TRx
%token <Ast_asm.fregister> TFx
%token TR TF
%token TPC TSB TFP TSP

%token TC
%token <Ast_asm5.creg> TCx

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
%token TC
%token TOPAR TCPAR

/*(*-----------------------------------------*)*/
/*(*2 Operators *)*/
/*(*-----------------------------------------*)*/

%token TPLUS TMINUS TTILDE TMUL TMOD
%token TSLASH

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

%type <Ast_asm5.instr_with_cond Ast_asm.lines> program
%start program

%%

/*(*************************************************************************)*/
/*(*1 Program (arch independent) *)*/
/*(*************************************************************************)*/

program: lines EOF { $1 }

lines: 
 | /*empty*/  { [] }
 | line lines { $1 @ $2 }

line:
 |               TSEMICOLON { [] }
 | instr         TSEMICOLON { [(Instr (fst $1, snd $1), $2)] }
 | pseudo_instr  TSEMICOLON { [(Pseudo $1, $2)] }
 /*(* claude: RET moved into `instr` below (as CRET) so it can carry a
    * real condition ("RET.MI") -- see CRET's own Ast_asm5.ml comment.
    * ARM's virtual_instr is otherwise unused (TNOP is a dead token
    * here, like several other arch's shared-but-unused tokens). *)*/
 /*(* claude: end-of-file marker (real 5a accepts a bare "END", no
    * operands) -- a true no-op. *)*/
 | TEND          TSEMICOLON { [] }

 | label_def line           { $1::$2 }

label_def: TIDENT TCOLON    { (LabelDef $1, !L.line) }

/*(*************************************************************************)*/
/*(*1 Pseudo instructions (arch independent) *)*/
/*(*************************************************************************)*/
/*(* I can't factorize in attr_opt; shift/reduce conflict with TC *)*/
pseudo_instr:
 | TTEXT  global TC imm    
     { TEXT  ($2, default_attr, $4) }
 | TGLOBL global TC imm    
     { GLOBL ($2, default_attr, $4) }

 /*(* less: would be better to have mnemonics for attributes too *)*/
 | TTEXT global TC con TC imm
     { TEXT ($2, attributes_of_int $4, $6) }
 | TGLOBL global TC con TC imm
     { GLOBL ($2, attributes_of_int $4, $6) }

 | TDATA global_and_offset TSLASH con TC ximm
     { DATA (fst $2, snd $2, $4, $6) }
 | TWORD ximm
     { WORD $2 }

 /*(* claude: MCR/MRC (coprocessor register move). Unlike every other
    * instruction, goken computes the final 32-bit word directly in
    * the grammar action itself (a.y's own comment: "MCR MRC"), no
    * codegen.c dispatch at all, and emits it as a plain WORD
    * pseudo-op -- so this reuses Ast_asm.WORD directly rather than
    * adding a new instr constructor (no AST/Object_file.version
    * change needed). Real ARM syntax: "MCR 15,0,R0,C1,C0,0"
    * (coprocessor#, opcode1, Rd, Crn, Crm, opcode2 -- opcode2
    * defaults to 0 via `oexpr` if omitted). Coprocessor access is
    * normally privileged (CP15 etc), though qemu-arm's user-mode
    * emulation actually implements some reads (e.g. MRC of the Main
    * ID Register) rather than trapping -- see mcr_mrc.s for the
    * details of what's byte-tested and what's only trap-tested. *)*/
 | TMCR cond con TC expr TC reg TC creg TC creg oexpr
     { let (R rd) = $7 in
       let (C crn) = $9 and (C crm) = $11 in
       let word =
         (0xe lsl 24)
         lor ($1 lsl 20)
         lor (Ast_asm5.int_of_condition $2 lsl 28)
         lor (($3 land 15) lsl 8)
         lor (($5 land 7) lsl 21)
         lor ((rd land 15) lsl 12)
         lor ((crn land 15) lsl 16)
         lor ((crm land 15) lsl 0)
         lor (($12 land 7) lsl 5)
         lor (1 lsl 4)
       in
       WORD (Int word)
     }

/*(* stricter: I introduced those intermediate rules *)*/
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
/*(*1 Instructions, arch specific!! *)*/
/*(*************************************************************************)*/

instr:
 /*(* claude: condf (not cond) since ".S" (set condition flags, e.g.
    * fmt/fmtfdflush.c's real 5c -S output "SUB.S R3,R2,R7" ahead of a
    * predicated "BEQ ...", or utf/rune.c's "AND.S $192,R1") is real,
    * ordinary ARM data-processing syntax every one of these opcodes
    * supports -- same missing-condf bug MOVE's own TMOV production
    * had (see Ast_asm5.move_opt_of_flags's comment), just for
    * arith_cond/gsetbit instead of move_cond. Only .S is meaningful
    * here (P/W/U/F don't apply to a plain arithmetic op) -- rejected
    * with a real error, same convention as MOVM/MOVE's own rejections.
    * See docs/claude_notes/plan_hello_libc_linking.md. *)*/
 | TARITH condf  imsr TC reg TC reg
     { let (c, flags) = $2 in
       if flags land (lnot Ast_asm5.sflag_sbit) <> 0
       then error "ARITH only supports .S, not .P/.W/.U/.F"
       else (Arith ($1, (if flags <> 0 then Some Set_condition else None),
                    $3, Some $5, $7), c)
     }
 | TARITH condf  imsr TC reg
     { let (c, flags) = $2 in
       if flags land (lnot Ast_asm5.sflag_sbit) <> 0
       then error "ARITH only supports .S, not .P/.W/.U/.F"
       else (Arith ($1, (if flags <> 0 then Some Set_condition else None),
                    $3, None, $5), c)
     }
 | TMVN   condf  imsr TC reg
     { let (c, flags) = $2 in
       if flags land (lnot Ast_asm5.sflag_sbit) <> 0
       then error "MVN only supports .S, not .P/.W/.U/.F"
       else (Arith (MVN, (if flags <> 0 then Some Set_condition else None),
                    $3, None, $5), c)
     }

 | TARITHF cond frcon         TC freg  { (ArithF ($1, $3, None, $5), $2) }
 | TARITHF cond frcon TC freg TC freg  { (ArithF ($1, $3, Some $5, $7), $2) }
 | TCMPF cond freg TC freg             { (CmpF ($1, $3, $5), $2) }
 | TMOVWF cond reg  TC freg            { (MOVWF ($1, $3, $5), $2) }
 | TMOVFW cond freg TC reg             { (MOVFW ($1, $3, $5), $2) }

 /*(* claude: condf (not cond) since ".P"/".W"/".S" all show up on
    * real 5c -S output: ".P"/".W" for post/pre-indexed writeback
    * addressing (e.g. memset's "MOVB.P R6,1(R5)" byte-fill loop),
    * ".S" for the classic ARM "test and move" idiom (e.g. dofmt.c's
    * "MOVW.S R0,R7" ahead of a predicated "MOVW.NE ...") -- see
    * Ast_asm5.move_opt_of_flags's own comment. .U/.F are never valid
    * for a plain MOVE (that's MOVBU/MOVHU's own, separate mnemonic,
    * and .F is MOVM-only), and at most one of P/W/S can be set at
    * once (unlike MOVM's PUW) -- both rejected here with a real
    * error, same convention as MOVM's own S/F rejection just below. *)*/
 | TMOV   condf  gen  TC gen
     { let (c, flags) = $2 in
       if flags land (Ast_asm5.sflag_ubit lor Ast_asm5.sflag_fbit) <> 0
       then error "MOVx.U/.F is not supported"
       else if (match Ast_asm5.move_opt_of_flags flags with
                | Some _ -> false | None -> flags <> 0)
       then error "at most one of MOVx.P/.W/.S is supported, not several at once"
       else (MOVE ($1, Ast_asm5.move_opt_of_flags flags, $3, $5), c)
     }

 /*(* case 50/51/52/53: MOVF/MOVD load/store -- same `gen` shape as
    * MOVW/MOVB/MOVH above (memory side via Indirect/Entity), `gen`
    * having grown a `freg` alternative below for the float-register
    * side. *)*/
 | TMOVF  cond  gen  TC gen     { (MOVEF ($1, $3, $5), $2) }

 | TSWAP  cond  reg  TC ireg    { (SWAP ($1, $5, $3, None), $2) }
 | TSWAP  cond  ireg TC reg     { (SWAP ($1, $3, $5, None), $2) }
 | TSWAP  cond  reg  TC ireg TC reg 
     { (SWAP ($1, $5, $3, Some $7), $2) }

 /*(*stricter: no cond here, use Bxx form, so normalized AST *)*/
 | TB        branch           { (B $2, AL) }
 | TBx       rel              { (Bxx ($1, $2), AL) }
 | TBL  cond branch           { (BL $3, $2)}
 | TCMP cond imsr TC reg  { (Cmp ($1, $3, $5), $2) }

 /*(* claude: switch-statement jump-table dispatch -- see
    * Ast_asm5.CASE/BCASE's own comment. CASE carries a real condition
    * (real 5c -S output always emits ".LS", but nothing here
    * hardcodes that); BCASE is a table entry, not a predicated
    * instruction, so no cond -- same shape as plain TB above. *)*/
 | TCASE  cond reg  { (CASE $3, $2) }
 | TBCASE branch    { (BCASE $2, AL) }

 | TSWI cond imm { (SWI $3, $2) }
 | TRFE cond     { (RFE, $2) }

 /*(* claude: RET, possibly conditional ("RET.MI" -- goken's real 5a
    * accepts this, confirmed empirically; a predicated early-return,
    * e.g. compiling "if(x<0) return -x;") -- see CRET's own
    * Ast_asm5.ml comment for why this replaced the old
    * unconditional-only `virtual_instr: TRET`. *)*/
 | TRET cond     { (CRET, $2) }

 /*(* case 17: "MULL cond R1,R2,(HI,LO)" *)*/
 | TMULL cond reg TC reg TC regreg
     { let (sign, accum) = $1 in
       let (hi, lo) = $7 in
       (MULL (sign, accum, $3, $5, hi, lo), $2)
     }

 /*(* case 38: "MOVM condf [reglist],oreg" -- stm (store: registers ->
    * memory). condf (not cond) since P/U/W address-mode suffixes are
    * how this instruction is actually written in practice (e.g.
    * ".DB.W" for a stack push); see movm_addr_mode's own comment.
    * Neither S nor F is supported: S is real on goken (the "load/
    * store user-mode registers" / "restore CPSR from SPSR" special
    * form) but privileged-only, with RFE already covering the one
    * exception-return use case that matters under this harness; F
    * is just goken's own C_UBIT/C_FBIT bit-packing accident aliased
    * onto MOVM (see sflag_fbit's comment in Ast_asm5.ml) -- both are
    * rejected here with a real error rather than silently doing
    * something a user wouldn't expect. *)*/
 | TMOVM condf TLBRACKET reglist TRBRACKET TC ioreg
     { let (c, flags) = $2 in
       if flags land (Ast_asm5.sflag_sbit lor Ast_asm5.sflag_fbit) <> 0
       then error "MOVM.S/.F is not supported"
       else (MOVM (Ast_asm5.movm_addr_mode_of_flags flags, RegList $4, $7), c)
     }
 /*(* case 39: "MOVM condf oreg,[reglist]" -- ldm (load: memory ->
    * registers). *)*/
 | TMOVM condf ioreg TC TLBRACKET reglist TRBRACKET
     { let (c, flags) = $2 in
       if flags land (Ast_asm5.sflag_sbit lor Ast_asm5.sflag_fbit) <> 0
       then error "MOVM.S/.F is not supported"
       else (MOVM (Ast_asm5.movm_addr_mode_of_flags flags, $3, RegList $6), c)
     }

/*(*************************************************************************)*/
/*(*1 Operands *)*/
/*(*************************************************************************)*/

imsr:
 | imm   { Imm $1 }
 | shift { $1 }
 | reg   { Reg $1 }


imm: TDOLLAR con      { $2 }

reg:
 | TRx                { $1 }
 /*(* stricter? could remove, redundant with cpp *)*/
 | TR TOPAR expr TCPAR 
     { if $3 <= 15 && $3 >= 0
       then R $3
       else error "register value out of range"
     }

/*(* for MULL (case 17): "(HI,LO)" *)*/
regreg: TOPAR reg TC reg TCPAR { ($2, $4) }

/*(* for MCR/MRC: "C1" (a coprocessor register). goken's a.y also
   * allows a computed "C(expr)" alternate spelling, but that's a
   * rarely-used form (real code just writes the numeric mnemonic);
   * not supported here, narrower but real, same precedent as PSR's
   * ".F" or MOVM's ".S". *)*/
creg: TCx { $1 }

/*(* for MCR/MRC: the optional trailing ",opcode2" -- defaults to 0
   * when omitted, matching goken's own `oexpr` rule. *)*/
oexpr:
 | /* empty */ { 0 }
 | TC expr     { $2 }

/*(* ARM specific *)*/
/*(* claude: a shifted-register as a generic operand to *any*
 * arithmetic instruction (not just the standalone SLL/SRL/SRA
 * mnemonics), e.g. real 5c -S output for fmt/dofmt.c's
 * "ADD R5->2,R2" or fmt/fmt.c's "ADD R5<<3,R3,R8". TSHL/TSHR/
 * TSHMINUS/TSHAT (single combined tokens) never actually got
 * produced by any lexer -- dead grammar. goken's own real 5a
 * (assemblers/5a/a.y's own `shift:` rule) doesn't lex these as
 * single tokens either: it combines two adjacent raw '<'/'>'/'-'
 * characters (TLT/TGT/TMINUS here) directly at the grammar level
 * ("regi '<' '<' rcon", "regi '-' '>' rcon", ...) -- so mirroring
 * that structure, not adding new lexer tokens, is both the fix and
 * genuine 5a-grammar parity (unlike CASE/BCASE or .CC/.CS elsewhere
 * in this port). Rotate-right ("@>", goken's `regi LAT '>' rcon`)
 * is left unimplemented -- no real -S output needing it has been
 * seen yet, and '@' isn't otherwise a standalone lexer token here
 * (only valid as an identifier's first character), so it would need
 * its own small lexer change first; see
 * docs/claude_notes/plan_hello_libc_linking.md. *)*/
shift:
 | reg TLT TLT rcon     { Shift ($1, Sh_logic_left, $4)  }
 | reg TGT TGT rcon     { Shift ($1, Sh_logic_right, $4)  }
 | reg TMINUS TGT rcon  { Shift ($1, Sh_arith_right, $4)  }

rcon:
 | reg { Left $1 }
 | con { if ($1 >= 0 && $1 <= 31)
         then Right $1 
         else error "shift value out of range" 
       }



gen:
 | ximm  { match $1 with Int x -> Imsr (Imm x) | x -> Ximm x }
 | shift { Imsr ($1) }
 | reg   { Imsr (Reg $1) }
 /*(* claude: the float-register side of a MOVF/MOVD (case 50-53) --
    * the only place a bare `freg` can appear as a `gen`/mov_operand
    * on its own (as opposed to freg's other uses, e.g. ArithF, which
    * don't go through `gen` at all). *)*/
 | freg  { FImsr $1 }
 /*(* case 56/57: "MOVW R0,FPSR" / "MOVW FPSR,R0" -- same MOVW
    * mnemonic/gen mechanism as ordinary int moves, dispatched by
    * operand shape in Codegen5.ml, not by grammar. *)*/
 | TFCR  { FCRImsr $1 }
 /*(* case 35/36/37: "MOVW CPSR,R0" / "MOVW R0,CPSR" / "MOVW $5,CPSR"
    * -- same MOVW mnemonic/gen mechanism as FCRImsr. *)*/
 | TPSR  { PSRImsr $1 }

 | ioreg { $1 }
 | name                    { Entity $1 }
 | con TOPAR pointer TCPAR { Entity ($3 None $1) }

ximm:
 | imm             { Int $1 }
 | fcon            { Float $1 }
 | TDOLLAR TSTRING { String $2 }
 | TDOLLAR name    { Address $2 }

ioreg:
 | ireg     { Indirect ($1, 0) }
 | con ireg { Indirect ($2, $1) }
 /*(* claude: a xix-only pipeline accommodation, NOT real 5a grammar
    * parity -- see Ast_asm5.IndirectShift's own comment. Real 5c -S
    * output for a scaled-register-offset memory address, e.g.
    * "MOVB R7<<0(R3),R3"; goken's own real 5a has no source-level
    * syntax for register-offset addressing at all. *)*/
 | reg TLT TLT rcon ireg    { IndirectShift ($1, Sh_logic_left, $4, $5) }
 | reg TGT TGT rcon ireg    { IndirectShift ($1, Sh_logic_right, $4, $5) }
 | reg TMINUS TGT rcon ireg { IndirectShift ($1, Sh_arith_right, $4, $5) }

ireg: TOPAR reg TCPAR { $2 }



branch:
 | rel               { $1 }
 | global            { ref (SymbolJump $1) }
 | ireg              { ref (IndirectJump $1) }
 /*(* claude: a xix-only pipeline accommodation, NOT real 5a grammar
    * parity -- real 5c -S output prints an indirect call as
    * "BL 0(R2)" (an explicit zero offset before the parens), but
    * goken's own real 5a grammar (assemblers/5a/a.y's `branch: ...
    * nireg`, where `nireg: name | ireg`) only accepts the bare
    * "(R2)" form for a branch target; confirmed empirically ("BL
    * 0(R2)" is rejected by goken's real 5a, "BL (R2)" is accepted).
    * Ordinary memory operands (oreg) DO allow a "0(Rn)" offset
    * prefix, so this is a real, narrow, branch-specific grammar
    * restriction Pconv's generic operand-printing doesn't know
    * about -- same category as CASE/BCASE and .CC/.CS elsewhere in
    * this port: widening o5a to accept it is deliberate leniency
    * scoped to this stress-testing pipeline, not something claimed
    * to match real 5a. See
    * docs/claude_notes/plan_hello_libc_linking.md. *)*/
 | con ireg          { if $1 <> 0 then error "offset before an indirect \
                          branch target is a xix-only accommodation for \
                          real 5c -S output, and only ever 0 there"
                        else ref (IndirectJump $2) }

rel:
 | TIDENT offset        { ref (LabelUse ($1, $2)) }
 | con TOPAR TPC TCPAR  { ref (Relative $1) }

/*(*-----------------------------------------*)*/
/*(*2 name and offset (arch independent)  *)*/
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
 | TSP  { (fun name_opt offset ->
           match name_opt with
           | None -> Param (None, offset)
           | Some ({name = s; priv = _false; signature = _}) -> Param (Some s, offset)
           )
         }
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
/*(*2 float *)*/
/*(*-----------------------------------------*)*/

freg:
 | TFx                { $1 }
 | TF TOPAR con TCPAR 
     { if $3 <= 15 && $3 >= 0
       then FR $3
       else error "register value out of range"
     }

frcon:
  | freg { Right $1 }
  | fcon { Left $1 }

/*(*-----------------------------------------*)*/
/*(*2 number constants and expressions (arch independent)  *)*/
/*(*-----------------------------------------*)*/

con:
 | TINT { $1 }

 | TMINUS con { - $2 }
 | TPLUS  con { $2 }
 | TTILDE con { lnot $2 }

 | TOPAR expr TCPAR { $2 }

fcon: 
 | TDOLLAR TFLOAT { $2 }
 | TDOLLAR TMINUS TFLOAT { -. $3 }

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

/*(*************************************************************************)*/
/*(*1 Misc *)*/
/*(*************************************************************************)*/

cond:
 | /* empty */ { AL }
 | TCOND  { $1 }

/*(* claude: case 38/39 -- the generic condition+suffix-flags
   * accumulator, directly mirroring goken's own left-recursive
   * `cond: empty | cond LCOND | cond LS` (a.y). Kept as its own
   * nonterminal (rather than changing `cond` itself, which every
   * other instruction above still uses) so that adding this doesn't
   * silently change what any existing, already-tested production
   * accepts -- only MOVM opts into flag parsing. A future case that
   * wants e.g. Arith's ".S" or MOVE's ".W"/".P" to be real would
   * switch that production to `condf` too, and decide there how to
   * handle/reject bits it doesn't understand, same as MOVM does
   * below for ".S"/".F". *)*/
condf:
 | /* empty */  { (AL, 0) }
 | condf TCOND  { let (_, flags) = $1 in ($2, flags) }
 | condf TSUF   { let (c, flags) = $1 in (c, flags lor $2) }

/*(* case 38/39: "[R4-R11,R14]" -- a plain register-bitmask, folded
   * left-to-right same as goken's own `reglist` rule (a.y). *)*/
reglist:
 | reg { let (R i) = $1 in 1 lsl i }
 | reg TMINUS reg
     { let (R a) = $1 and (R b) = $3 in
       let lo = min a b and hi = max a b in
       let bits = ref 0 in
       for i = lo to hi do bits := !bits lor (1 lsl i) done;
       !bits
     }
 | reg TC reglist { let (R i) = $1 in (1 lsl i) lor $3 }

/*(*e: Parser_asm5.mly *)*/
