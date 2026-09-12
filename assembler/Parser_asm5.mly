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
%token <Ast_asm5.cmp_opcode> TCMP   
%token <Ast_asm5.condition> TBx TCOND
%token TSWI TRFE
%token TMOVM
/*(* case 38/39: the generic dot-suffix-flag token (P/U/W/S/F bits,
   * see Ast_asm5.ml's sflag_* comment) -- only MOVM's `condf` rule
   * (below) folds these in for now; every other instruction still
   * uses the plain `cond` rule unchanged, so this can't silently
   * change behavior of anything already working. *)*/
%token <int> TSUF
%token TLBRACKET TRBRACKET

%token TRET TNOP

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
 | virtual_instr TSEMICOLON { [(Virtual $1, $2)] }

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
/*(*1 Virtual instructions (arch independent) *)*/
/*(*************************************************************************)*/

virtual_instr:
 /*(* was in instr before. stricter: no cond (nor comma) *)*/
 | TRET                  { RET }

/*(*************************************************************************)*/
/*(*1 Instructions, arch specific!! *)*/
/*(*************************************************************************)*/

instr:
 | TARITH cond  imsr TC reg TC reg 
     { (Arith ($1, None, $3, Some $5, $7), $2) }
 | TARITH cond  imsr TC reg  { (Arith ($1,  None, $3, None, $5), $2) }
 | TMVN   cond  imsr TC reg  { (Arith (MVN, None, $3, None, $5), $2) }

 | TARITHF cond frcon         TC freg  { (ArithF ($1, $3, None, $5), $2) }
 | TARITHF cond frcon TC freg TC freg  { (ArithF ($1, $3, Some $5, $7), $2) }
 | TCMPF cond freg TC freg             { (CmpF ($1, $3, $5), $2) }
 | TMOVWF cond reg  TC freg            { (MOVWF ($1, $3, $5), $2) }
 | TMOVFW cond freg TC reg             { (MOVFW ($1, $3, $5), $2) }

 | TMOV   cond  gen  TC gen     { (MOVE ($1, None, $3, $5), $2) }

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

 | TSWI cond imm { (SWI $3, $2) }
 | TRFE cond     { (RFE, $2) }

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

/*(* ARM specific *)*/
shift:
 | reg TSHL rcon     { Shift ($1, Sh_logic_left, $3)  }
 | reg TSHR rcon     { Shift ($1, Sh_logic_right, $3)  }
 | reg TSHMINUS rcon { Shift ($1, Sh_arith_right, $3)  }
 | reg TSHAT rcon    { Shift ($1, Sh_rotate_right, $3)  }

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

ireg: TOPAR reg TCPAR { $2 }



branch: 
 | rel               { $1 }
 | global            { ref (SymbolJump $1) }
 | ireg              { ref (IndirectJump $1) }

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
