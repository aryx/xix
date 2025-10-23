%{
(* Copyright 2015, 2016 Yoann Padioleau, see copyright.txt *)
open Common
open Either

open Ast_asm
open Ast_asmv
module L = Location_cpp

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* 
 * Limitations compared to va (see also Parser_asm5.mly comment):
 *  - 
 * todo:
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

%token TSYSCALL TRFE TBREAK

%token TRET TNOP

%token TTEXT TGLOBL 
%token TDATA TWORD 

/*(*-----------------------------------------*)*/
/*(*2 registers *)*/
/*(*-----------------------------------------*)*/

%token <Ast_asm5.register> TRx
%token <Ast_asm5.fregister> TFx
%token TR TF
%token TPC TSB TFP TSP

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

%type <Ast_asmv.program> program
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
 | instr         TSEMICOLON { [(Instr (fst $1, snd $1), $2)] }
 | pseudo_instr  TSEMICOLON { [(Pseudo $1, $2)] }
 | virtual_instr TSEMICOLON { [(Virtual $1, $2)] }

 | label_def line           { $1::$2 }

label_def: TIDENT TCOLON    { (LabelDef $1, !L.line) }

/*(*************************************************************************)*/
/*(*1 Pseudo instructions (same than in Parser_asm5.mly) *)*/
/*(*************************************************************************)*/
pseudo_instr:
 | TTEXT  global TC imm    
     { TEXT  ($2, noattr, $4) }
 | TGLOBL global TC imm    
     { GLOBL ($2, noattr, $4) }

 /*(* less: would be better to have mnemonics for attributes too *)*/
 | TTEXT global TC con TC imm
     { TEXT ($2, attributes_of_int $4, $6) }
 | TGLOBL global TC con TC imm
     { GLOBL ($2, attributes_of_int $4, $6) }

 | TDATA global_and_offset TSLASH con TC ximm  
     { DATA (fst $2, snd $2, $4, $6) }


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
 /*(* was in instr before. stricter: no cond (nor comma) *)*/
 | TRET                  { RET }

/*(*************************************************************************)*/
/*(*1 Instructions *)*/
/*(*************************************************************************)*/

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

gen:
 | reg   { Imsr (Reg $1) }

ximm:
 | imm             { Left $1 }
 /*(* todo: float *)*/
 | TDOLLAR TSTRING { Right (String $2) }
 | TDOLLAR name    { Right (Address $2) }


ireg: TOPAR reg TCPAR { $2 }

rel:
 | TIDENT offset        { ref (LabelUse ($1, $2)) }
 | con TOPAR TPC TCPAR  { ref (Relative $1) }

/*(*-----------------------------------------*)*/
/*(*2 name and offset (arch independent)  *)*/
/*(*-----------------------------------------*)*/

/*(*-----------------------------------------*)*/
/*(*2 Integer constant and expression (arch independent)  *)*/
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

/*(*************************************************************************)*/
/*(*1 Misc *)*/
/*(*************************************************************************)*/
