%{
(* Copyright 2025 Yoann Padioleau, see copyright.txt *)
open Common
open Either

open Ast_asm
open Ast_asmv
open Parser_asm
module L = Location_cpp

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* 
 * Limitations compared to va (see also Parser_asm.ml top comment):
 *  - 
 * todo:
 *  - SCHED/NOSCHED ?
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

%token <Ast_asmv.arith_opcode> TARITH
%token <Ast_asmv.arithf_opcode * Ast_asm.floatp_precision> TARITHF
%token <Ast_asmv.fcvt_dir> TFCVT
%token TNOR
%token <Ast_asmv.mul_opcode> TMULOP
%token TSYSCALL TRFE TBREAK
%token TSC TLL
%token TJMP TJAL
%token TBEQ TBNE
%token <Ast_asmv.b_condition> TB
%token <bool> TBFP
%token <Ast_asmv.tlb_kind> TTLB
%token <Ast_asmv.move1_size> TMOVE1
%token <Ast_asmv.move2_size> TMOVE2
%token <Ast_asmv.lohireg> TLOHI

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

%token TM TFCR
%token <Ast_asmv.mreg> TMx
%token <Ast_asmv.fcrreg> TFCRx

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
/*(* unused here; only Parser_asm5.mly's MOVM grammar needs these,
   * but the token is shared in Token_asm.ml so every arch's
   * Parse_asmX.ml must still translate it *)*/
%token TLBRACKET TRBRACKET
%token TBANG
/*(* claude: see Parser_asm5.mly's identical comment. *)*/
%token TLT TGT

/*(*-----------------------------------------*)*/
/*(*2 Operators *)*/
/*(*-----------------------------------------*)*/

%token TSHL TSHR   TSHMINUS TSHAT
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

%type <Ast_asmv.instr Ast_asm.lines> program
%start program

%%

/*(*************************************************************************)*/
/*(*1 Program (same than in Parser_asm5.mly) *)*/
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
 /*(* claude: end-of-file marker, real vc/va -S output always emits
    * it ("END\t,") -- a true no-op. *)*/
 | TEND          TSEMICOLON { [] }

 | label_def line           { $1::$2 }

label_def: TIDENT TCOLON    { (LabelDef $1, !L.line) }

/*(*************************************************************************)*/
/*(*1 Pseudo instructions (same than in Parser_asm5.mly) *)*/
/*(*************************************************************************)*/
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
/*(*1 Virtual instructions *)*/
/*(*************************************************************************)*/
virtual_instr:
 /*(* was in instr before *)*/
 | TRET                  { RET }

/*(*************************************************************************)*/
/*(*1 Instructions *)*/
/*(*************************************************************************)*/
instr:
 | TARITH imr TC reg TC reg     { Arith ($1, $2, Some $4, $6) }
 | TARITH imr        TC reg     { Arith ($1, $2, None, $4) }
 | TNOR   imr TC reg TC imr     { NOR ($2, Some $4, $6) }
 | TNOR   imr        TC imr     { NOR ($2, None, $4) }
 | TMULOP reg TC reg TC reg     { ArithMul ($1, $2, Some $4, $6) }
 | TMULOP reg        TC reg     { ArithMul ($1, $2, None, $4) }

 | TARITHF freg         TC freg { ArithF ($1, $2, None, $4) }
 | TARITHF freg TC freg TC freg { ArithF ($1, $2, Some $4, $6) }
 /*(* claude: real goken grammar is the same generic LTYPE5
    * "vlgen,vgen" shape as Move2 (see Ast_asmv.ml's own FCvt
    * comment for why this isn't wired through Move2 itself). *)*/
 | TFCVT freg TC freg           { FCvt ($1, $2, $4) }

 /*(* TODO? check "one side must be register" but va code buggy I think *)*/
 | TMOVE1 lgen TC gen           { Move1 ($1, $2, $4) }
 | TMOVE2 vlgen TC vgen         { Move2 ($1, $2, $4) }
 /*(* claude: real goken case 3 ("mov $soreg,r ==> or/add $i,o,r") --
    * "$off(Rbase)" as a Move2 *source* isn't a memory load at all,
    * it's "compute the EFFECTIVE ADDRESS Rbase+off into Rd" (an
    * address-of, like C's "&x"), a genuinely different instruction
    * shape (ADD-immediate) goken's own real assembler synthesizes
    * for this "mov"-spelled pseudo-op -- confirmed real, not a xix-
    * only accommodation (unlike the "BL 0(R2)" family): real va
    * accepts "MOVW $4(R29),R2" directly (MOVW lexes as TMOVE2, not
    * TMOVE1 -- confirmed by checking Parse_asmv.ml's own keyword
    * table before wiring this into the wrong production first), and
    * goken's own linkers/vl/asm.c case 3 shows the real ADDU/OR
    * encoding. Not representable as this port's own shared
    * Ast_asm.ximm (whose `Address` only wraps Param/Local/Global,
    * never a plain arbitrary register+offset), so this bypasses
    * Move2 entirely and produces the real Arith instruction goken's
    * own case 3 emits directly, sidestepping any shared-type change
    * (every real occurrence in a real closure so far uses R29/SP as
    * the base, but goken's own case 3 is fully general, so this is
    * too). The AOR micro-optimization goken's case 3 takes for
    * certain AND-mask-shaped constants isn't replicated (always
    * ADDU here) -- functionally identical, just not always byte-
    * identical for that narrow constant shape. Found stress-testing
    * real lib_core/libc (fmt/nan64.c's real "MOVW $4(R29),R2"). *)*/
 | TMOVE2 TDOLLAR con TOPAR reg TCPAR TC reg
     { Arith (ADD (W, A.U), Imm $3, Some $5, $8) }

 | TJMP branch { JMP $2 }
 | TJAL branch { JAL $2 }
 /*(* was just nireg here for branch *)*/
 | TJAL reg TC branch { JALReg ($2, $4) }
 | TRFE branch { RFE $2 }

 | TBEQ gen TC rel           { BEQ ($2, None, $4) }
 | TBNE gen TC rel           { BNE ($2, None, $4) }
 | TBEQ gen TC reg TC rel    { BEQ ($2, Some $4, $6) }
 | TBNE gen TC reg TC rel    { BNE ($2, Some $4, $6) }

 | TB gen TC rel             { Bxx ($1, $2, $4) }
 /*(* claude: real goken grammar `LTYPEG comma rel` -- no register
    * operand at all, unlike every other case-6 branch above. See
    * Ast_asmv.ml's own BFP comment. *)*/
 | TBFP rel                  { BFP ($1, $2) }

 | TSYSCALL { SYSCALL }
 | TLL gen TC reg { LL ($2, $4) }
 | TSC reg TC gen { SC ($2, $4) }
 | TTLB { TLB $1 }

/*(*************************************************************************)*/
/*(*1 Operands *)*/
/*(*************************************************************************)*/

imr:
 | imm   { Imm $1 }
 | reg   { Reg $1 }

imm: TDOLLAR con      { $2 }

reg:
 | TRx                { $1 }
 /*(* stricter? could remove, redundant with cpp *)*/
 | TR TOPAR expr TCPAR 
     { if $3 <= 31 && $3 >= 0
       then R $3
       else error "register value out of range"
     }

/*(*TODO: far more cases *)*/
gen:
 | reg   { GReg $1 }
 | con TOPAR reg TCPAR { Indirect ($3, $1) }
 | name  { Entity $1 }

ximm:
 | imm             { Int $1 }
 | fcon            { Float $1 }
 | TDOLLAR TSTRING { String $2 }
 | TDOLLAR name    { Address $2 }

lgen:
 | gen { Left $1 }
 | ximm { Right $1 }

ireg: TOPAR reg TCPAR { $2 }

branch:
 | rel               { $1 }
 | global            { ref (SymbolJump $1) }
 | ireg              { ref (IndirectJump $1) }
 /*(* claude: real va grammar, unlike ARM32's own "BL 0(R2)"
    * accommodation -- goken's real `assemblers/va/a.y` has `nireg:
    * ireg | con ireg` (MIPS's own indirect-branch nonterminal is
    * genuinely more permissive than ARM's `nireg: name | ireg`, no
    * "con" alternative there at all), and real `va` accepts "JAL
    * 0(R3)" directly (confirmed empirically). The constant is
    * accepted but discarded here, matching real goken's own case-18
    * codegen (`linkers/vl/asm.c`'s `OP_RRR(oprrr(p->as), 0,
    * p->to.reg, r)`) which never reads p->to.offset at all -- an
    * indirect JALR/JR has no immediate-offset field on real
    * hardware, so any constant here is purely a real 5c -S printing
    * artifact (always 0 in practice), not something with its own
    * encoding to get right. Found stress-testing real lib_core/libc
    * (fmt/dofmt.c's real "JAL 0(R3)", a call through a function
    * pointer). *)*/
 | con ireg          { ignore $1; ref (IndirectJump $2) }

rel:
 | TIDENT offset        { ref (LabelUse ($1, $2)) }
 | con TOPAR TPC TCPAR  { ref (Relative $1) }

/*(*TODO: far more cases *)*/
vgen:
 | gen { Gen $1 }
 | TLOHI { LoHi $1 }
 | freg { GFReg $1 }
 | mreg { MReg $1 }
 | fcrreg { FCReg $1 }

/*(*TODO: far more cases *)*/
vlgen:
 | lgen { match $1 with Left x -> Left (Gen x) | Right x -> Right x }
 | TLOHI { Left (LoHi $1) }
 | freg { Left (GFReg $1) }
 | mreg { Left (MReg $1) }
 | fcrreg { Left (FCReg $1) }

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
/*(*2 float (arch independent; check is arch dependent) *)*/
/*(*-----------------------------------------*)*/

freg:
 | TFx                { $1 }
 | TF TOPAR con TCPAR
     { if $3 <= 32 && $3 >= 0
       then FR $3
       else error "register value out of range"
     }

mreg:
 | TMx                { $1 }
 | TM TOPAR con TCPAR
     { if $3 <= 31 && $3 >= 0
       then M $3
       else error "register value out of range"
     }

fcrreg:
 | TFCRx              { $1 }
 | TFCR TOPAR con TCPAR
     { if $3 <= 31 && $3 >= 0
       then FCR $3
       else error "register value out of range"
     }

/*(*-----------------------------------------*)*/
/*(*2 number constant and expression (arch independent)  *)*/
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
