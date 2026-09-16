%{
(* Claude Code, Copyright (C) 2026 Yoann Padioleau, see copyright.txt *)
open Common
open Either

open Ast_asm
open Ast_asm7
open Parser_asm
module L = Location_cpp

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* The 7a ARM64/AArch64 assembly grammar -- see Ast_asm7.ml's own prelude
 * for the overall scope of this first version (SIMD, atomics, system
 * instructions, CSEL family, bitfield-move mnemonics, floating point,
 * load/store pair, and extended-register/pre-post-increment addressing
 * are all deferred).
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

%token <Ast_asm7.arith_opcode> TARITH
%token <Ast_asm7.shift_opcode> TSHIFT
%token <Ast_asm7.cmp_opcode> TCMP
%token <Ast_asm7.mul_opcode> TMULOP
%token <Ast_asm7.fp_arith_opcode> TFARITH
%token <Ast_asm7.fp_cmp_opcode> TFCMP
%token <Ast_asm7.barrier_opcode> TDMB
%token <Ast_asm7.cond_sel_opcode> TCONDSEL
%token <Ast_asm7.cond_set_opcode> TCONDSET
%token <Ast_asm7.neg2_opcode> TNEG2
%token <Ast_asm7.extend_opcode> TEXTEND
%token <Ast_asm7.rem_opcode> TREM
%token <Ast_asm7.move_size> TMOV
%token TB TBL
%token <Ast_asm7.condition> TBx
%token <bool> TCBx
%token <bool> TTBx
%token TCASE TBCASE
%token <Ast_asm7.condition> TCOND
%token TRET
%token TNOP
%token TEND
%token TSVC
%token <bool> TLDXR
%token <bool> TSTXR
/*(* claude: goken's compiler-facing "RETURN" pseudo-op -- distinct
   * from the real hardware "RET" instruction above. RETURN is expanded
   * by Rewrite7.ml (leaf/frame-size-driven prologue+epilogue
   * synthesis, mirroring goken's own noop.c) into the shared
   * Ast_asm.virtual_instr.RET, same convention ARM32/MIPS/RISC-V
   * already use for their own (compiler-only) RET. *)*/
%token TRETURN

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
%token TLBRACKET TRBRACKET
/*(* claude: pre/post-index writeback addressing (e.g. "-16(RSP)!" /
   * "(RSP)16!") -- needed for the link-register save/restore Rewrite7.ml
   * synthesizes for RETURN. *)*/
%token TBANG

/*(* claude: see Parser_asm5.mly's identical comment. *)*/
%token TLT TGT

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

%type <Ast_asm7.instr Ast_asm.lines> program
%start program

%%

/*(*************************************************************************)*/
/*(*1 Program (arch independent, same as Parser_asm5.mly/Parser_asmi.mly) *)*/
/*(*************************************************************************)*/

program: lines EOF { $1 }

lines:
 | /*empty*/  { [] }
 | line lines { $1 @ $2 }

line:
 |               TSEMICOLON { [] }
 | instr         TSEMICOLON { [(Instr $1, $2)] }
 | pseudo_instr  TSEMICOLON { [(Pseudo $1, $2)] }
 | virtual_instr TSEMICOLON { [(Virtual $1, $2)] }
 /*(* claude: end-of-file marker, real compiler -S output always
    * emits it ("END\t,") -- a true no-op. *)*/
 | TEND          TSEMICOLON { [] }

 | label_def line           { $1::$2 }

label_def: TIDENT TCOLON    { (LabelDef $1, !L.line) }

/*(*************************************************************************)*/
/*(*1 Virtual instructions (arch independent) *)*/
/*(*************************************************************************)*/
virtual_instr:
 /*(* claude: goken's compiler-facing "RETURN" pseudo-op -- see TRETURN's
    * own comment. *)*/
 /*(* claude: recent OCaml's type-directed disambiguation would resolve
    * a bare "RET" here to Ast_asm.RET (the type expected at this
    * production) even with Ast_asm7 also `open`ed above and also
    * defining its own RET of reg option -- ocaml-light's ocamlyacc/
    * ocamlc combo doesn't do that disambiguation from context here,
    * picks the last-opened module's RET instead, and fails to
    * typecheck ("RET expects 1 argument"). Qualified explicitly. *)*/
 | TRETURN                  { Ast_asm.RET }

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
 /*(* case 1/4: "ADD $imm,[Rn,]Rd" / "ADD Rm,[Rn,]Rd" *)*/
 | TARITH imr TC reg TC reg     { Arith ($1, $2, Some $4, $6) }
 | TARITH imr        TC reg     { Arith ($1,  $2, None, $4) }

 /*(* case 8/9: "LSL $c,[Rn,]Rd" (bitfield encoding) / "LSL Rm,[Rn,]Rd" *)*/
 | TSHIFT imr TC reg TC reg     { Shift ($1, $2, Some $4, $6) }
 | TSHIFT imr        TC reg     { Shift ($1,  $2, None, $4) }

 /*(* case 7: "CMP $imm,Rn" / "CMP Rm,Rn" *)*/
 | TCMP imr TC reg              { Cmp ($1, $2, $4) }

 /*(* case 15: "MUL Rm,[Rn,]Rd" *)*/
 | TMULOP reg TC reg TC reg     { ArithMul ($1, $2, Some $4, $6) }
 | TMULOP reg        TC reg     { ArithMul ($1, $2, None, $4) }

 /*(* case 16: "REM Rdivisor,Rdividend,Rdest" / "REM Rdivisor,Rdest" *)*/
 | TREM reg TC reg TC reg       { Rem ($1, $2, Some $4, $6) }
 | TREM reg        TC reg       { Rem ($1, $2, None, $4) }

 /*(* case 3: "MOV(B[U]|H[U]|W[U])? gen,gen" -- covers register move,
    * register<->memory, and register<->immediate, dispatched by operand
    * shape at codegen time. *)*/
 | TMOV lgen TC gen             { Move ($1, $2, $4) }

 /*(* case 5/6: "B label" / "B (Rn)" / "BL label" / "BL (Rn)" *)*/
 | TB branch                    { B $2 }
 | TBL branch                   { BL $2 }

 /*(* case 7 (branch variant): "BEQ label" etc *)*/
 | TBx rel                      { Bxx ($1, $2) }

 /*(* case 8 (its own LTYPE, unrelated to Shift's case 8): "CBZ
    * Rt,label" / "CBNZ Rt,label" *)*/
 | TCBx reg TC rel              { CBxx ($1, $2, $4) }

 /*(* case: bare "RET" or "RET Rn" *)*/
 | TRET                         { RET None }
 | TRET reg                     { RET (Some $2) }

 /*(* case 10: "SVC" or "SVC $imm" *)*/
 | TSVC                         { SVC 0 }
 | TSVC imm                     { SVC $2 }

 /*(* case 54: "FADDD Fm,[Fn,]Fd" *)*/
 | TFARITH freg TC freg TC freg { FArith ($1, $2, Some $4, $6) }
 | TFARITH freg        TC freg  { FArith ($1, $2, None, $4) }

 /*(* case 56: "FCMPD Fm,Fn" *)*/
 | TFCMP freg TC freg           { FCmp ($1, $2, $4) }

 /*(* case 24/25: "NEG Rn,Rd" / "MVN Rn,Rd" (plain-register form only) *)*/
 | TNEG2 reg TC reg             { Neg2 ($1, $2, $4) }

 /*(* case 45: "SXTW Rn,Rd" / "UXTW Rn,Rd" *)*/
 | TEXTEND reg TC reg           { Extend ($1, $2, $4) }

 /*(* case 51: "DMB $imm" / "DSB $imm" / "ISB $imm" *)*/
 | TDMB imm                     { Barrier ($1, $2) }

 /*(* case 40: "TBZ $bit,Rt,label" / "TBNZ $bit,Rt,label" *)*/
 | TTBx imm TC reg TC rel       { TBxx ($1, $2, $4, $6) }

 /*(* case 62/63: "CASE Rv,Rt" / "BCASE label" -- real 7a grammar
    * (LTYPED/LTYPE5 in a.y), see Ast_asm7.ml's CaseJump/BCase
    * comment. *)*/
 | TCASE reg TC reg             { CaseJump ($2, $4) }
 | TBCASE rel                   { BCase $2 }

 /*(* case 18: "CSEL EQ,Rn,Rm,Rd" / "CINC EQ,Rn,Rd" (2-register alias
    * form -- see CondSel's own AST comment) *)*/
 | TCONDSEL cond TC reg TC reg TC reg { CondSel ($1, $2, $4, Some $6, $8) }
 | TCONDSEL cond TC reg TC reg        { CondSel ($1, $2, $4, None, $6) }

 /*(* case 18: "CSET EQ,Rd" *)*/
 | TCONDSET cond TC reg         { CondSet ($1, $2, $4) }

 /*(* case 58: "LDXR (Rn),Rt" / "LDAXR (Rn),Rt" *)*/
 | TLDXR ireg TC reg            { LoadExcl ($1, $2, $4) }
 /*(* case 59: "STXR Rt,(Rn),Rs" / "STLXR Rt,(Rn),Rs" -- confirmed this
    * exact operand order empirically against real goken (see
    * Ast_asm7.ml's StoreExcl comment), not assumed from the grammar
    * alone. *)*/
 | TSTXR reg TC ireg TC reg     { StoreExcl ($1, $2, $4, $6) }

/*(*************************************************************************)*/
/*(*1 Operands *)*/
/*(*************************************************************************)*/

imr:
 | imm   { Imm $1 }
 | reg   { Reg $1 }

imm: TDOLLAR con      { $2 }

reg:
 | TRx                { $1 }
 | TR TOPAR expr TCPAR
     { if $3 <= 31 && $3 >= 0
       then R $3
       else error "register value out of range"
     }

freg:
 | TFx                { $1 }
 | TF TOPAR expr TCPAR
     { if $3 <= 31 && $3 >= 0
       then FR $3
       else error "register value out of range"
     }

/*(* claude: needed for e.g. "MOVW 8(R2),R5" (case 3's Indirect memory
   * side) -- same shape as Parser_asmi.mly/Parser_asmv.mly's identical
   * rule. The "(Rn)(Rm)" register-offset form is still deferred, see
   * Ast_asm7.ml's prelude comment; the "!" pre/post-index writeback
   * forms (goken's D_XPRE/D_XPOST) are wired below, needed for
   * Rewrite7.ml's RETURN-expansion link-register save/restore. *)*/
gen:
 | reg                 { GReg $1 }
 | freg                { GFReg $1 }
 | con TOPAR reg TCPAR { Indirect ($3, $1) }
 | name                { Entity $1 }
 /*(* case 22/23-ish: "-16(RSP)!" -- pre-index, offset applied before
    * the access, base register updated afterward. *)*/
 | con TOPAR reg TCPAR TBANG { PreIndex ($3, $1) }
 /*(* "(RSP)16!" -- post-index, offset applied (and base updated)
    * after the access. *)*/
 | TOPAR reg TCPAR con TBANG { PostIndex ($2, $4) }

ximm:
 | imm             { Int $1 }
 | fcon            { Float $1 }
 | TDOLLAR TSTRING { String $2 }
 | TDOLLAR name    { Address $2 }

lgen:
 | gen  { Left $1 }
 | ximm { Right $1 }

ireg: TOPAR reg TCPAR { $2 }

/*(* claude: bare condition-code operand (goken's own `cond:` rule,
   * a.y: "LCOND { $$.type=D_COND; $$.reg=$1; }") -- a genuinely
   * different grammar/lexer slot from `TBx`'s "BEQ"/"BNE"/... branch
   * mnemonics, even though both ultimately carry the same
   * Ast_asm7.condition payload; see Parse_asm7.ml's keyword table for
   * the separate "EQ"/"NE"/... entries this produces. *)*/
cond: TCOND { $1 }

branch:
 | rel               { $1 }
 | global            { ref (SymbolJump $1) }
 | ireg              { ref (IndirectJump $1) }
 /*(* claude: a xix-only pipeline accommodation, NOT real 7a grammar
    * parity -- same issue, same fix, as ARM32's own Parser_asm5.mly
    * "con ireg" branch rule (see its own comment for the full
    * reasoning): real 5c/7c -S output prints an indirect call as "BL
    * 0(R2)" (an explicit zero offset before the parens), but goken's
    * real 7a grammar (assemblers/7a/a.y's `nireg: '(' sreg ')' |
    * name`) only accepts the bare "(R2)" form. Found stress-testing
    * against real lib_core/libc (fmt/dofmt.c's real "BL 0(R2)"), see
    * docs/claude_notes/plan_hello_libc_linking.md. *)*/
 | con ireg          { if $1 <> 0 then error "offset before an indirect \
                          branch target is a xix-only accommodation for \
                          real 7c -S output, and only ever 0 there"
                        else ref (IndirectJump $2) }

rel:
 | TIDENT offset        { ref (LabelUse ($1, $2)) }
 | con TOPAR TPC TCPAR  { ref (Relative $1) }

/*(*-----------------------------------------*)*/
/*(*2 name and offset (arch independent)  *)*/
/*(*-----------------------------------------*)*/

name:
 | TIDENT         offset TOPAR pointer TCPAR { $4 (Some (mk_g $1 false)) $2 }
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
/*(*2 float (arch independent; not wired for real use yet, see
   * Ast_asm7.ml's prelude comment) *)*/
/*(*-----------------------------------------*)*/

fcon:
 | TDOLLAR TFLOAT         { $2 }
 | TDOLLAR TMINUS TFLOAT  { -. $3 }

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
