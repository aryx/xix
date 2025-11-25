%{
(* Copyright 2025 Yoann Padioleau, see copyright.txt *)
open Common
open Either

open Ast_asm
open Ast_asmi
open Parser_asm
module L = Location_cpp

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* 
 * Limitations compared to ia/ja (see also Parser_asm.ml top comment):
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

%token <Ast_asmi.arith_opcode> TARITH
%token <Ast_asmi.arithf_opcode * Ast_asm.floatp_precision> TARITHF
%token <Ast_asmi.mul_opcode> TMULOP
%token TSYSCALL TRFE TBREAK
%token TJMP TJAL
%token TBEQ TBNE
%token <Ast_asmi.b_condition> TB
%token <Ast_asmi.move1_size> TMOVE1
%token <Ast_asmi.move2_size> TMOVE2

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

/*
%token TM TFCR
%token <Ast_asmi.mreg> TMx
%token <Ast_asmi.fcrreg> TFCRx
*/

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

%type <Ast_asmi.instr Ast_asm.lines> program
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

 | TMULOP reg TC reg TC reg     { ArithMul ($1, $2, Some $4, $6) }
 | TMULOP reg        TC reg     { ArithMul ($1, $2, None, $4) }

 | TARITHF freg         TC freg { ArithF ($1, $2, None, $4) }
 | TARITHF freg TC freg TC freg { ArithF ($1, $2, Some $4, $6) }

 /*(* TODO? check "one side must be register" but va code buggy I think *)*/
 | TMOVE1 lgen TC gen           { Move1 ($1, $2, $4) }
 | TMOVE2 vlgen TC vgen         { Move2 ($1, $2, $4) }

 | TJMP branch { JMP $2 }
 | TJAL branch { JAL $2 }
 /*(* was just nireg here for branch *)*/
 | TJAL reg TC branch { JALR ($2, $4) }

 | TB gen TC rel             { Bxx ($1, $2, $4) }

 | TSYSCALL { ECALL }

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

rel:
 | TIDENT offset        { ref (LabelUse ($1, $2)) }
 | con TOPAR TPC TCPAR  { ref (Relative $1) }

/*(*TODO: far more cases *)*/
vgen:
 | gen { Gen $1 }

/*(*TODO: far more cases *)*/
vlgen:
 | lgen { match $1 with Left x -> Left (Gen x) | Right x -> Right x }

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
/*(*2 float (arch independent; check is arch dependent) *)*/
/*(*-----------------------------------------*)*/

freg:
 | TFx                { $1 }
 | TF TOPAR con TCPAR 
     { if $3 <= 32 && $3 >= 0
       then FR $3
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
