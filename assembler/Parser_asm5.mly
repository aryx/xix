%{
(* Copyright 2015, 2016 Yoann Padioleau, see copyright.txt *)
open Common
open Either

open Ast_asm
open Ast_asm5
module L = Location_cpp

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* 
 * todo:
 *  - special bits
 *  - advanced instructions
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let error s =
  raise (L.Error (spf "Syntax error: %s" s, !L.line))


let noattr = { dupok = false; prof = true }

(* less: should use keywords in Asm5 instead of abusing integers *)
let attributes_of_int i =
   match i with 
   | 0 -> noattr
   (* NOPROF *)
   | 1 -> { dupok = false; prof = false }
   (* DUPOK *)
   | 2 -> { dupok = true; prof = true }
   (* both DUPOK and NOPROF *)
   | 3 -> { dupok = true; prof = false }

   | _ -> error (spf "unknown attribute or attribute combination: %d" i)

let mk_e name static = 
  { name; priv = if static then Some (-1) else None; signature = None; }

%}

/*(*************************************************************************)*/
/*(*1 Tokens *)*/
/*(*************************************************************************)*/

/*(*-----------------------------------------*)*/
/*(*2 opcodes *)*/
/*(*-----------------------------------------*)*/

%token <Ast_asm5.arith_opcode> TARITH
%token TMVN
%token <Ast_asm.move_size> TMOV TSWAP
%token TB  TBL
%token TRET TNOP
%token <Ast_asm5.cmp_opcode> TCMP   
%token <Ast_asm5.condition> TBx TCOND
%token TSWI TRFE

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

%token TCOLON TDOT TCOMMA TDOLLAR
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

%type <Ast_asm5.program> program
%start program

%%

/*(*************************************************************************)*/
/*(*1 Program *)*/
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
/*(*1 Pseudo instructions *)*/
/*(*************************************************************************)*/
/*(* I can't factorize in attr_opt; shift/reduce conflict with TCOMMA *)*/
pseudo_instr:
 | TTEXT  global TCOMMA imm    
     { TEXT  ($2, noattr, $4) }
 | TGLOBL global TCOMMA imm    
     { GLOBL ($2, noattr, $4) }

 /*(* less: would be better to have mnemonics for attributes too *)*/
 | TTEXT global TCOMMA con TCOMMA imm
     { TEXT ($2, attributes_of_int $4, $6) }
 | TGLOBL global TCOMMA con TCOMMA imm
     { GLOBL ($2, attributes_of_int $4, $6) }

 | TDATA global_and_offset TSLASH con TCOMMA ximm  
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
 /*(* was in instr before *)*/
 | TRET                  { RET }

/*(*************************************************************************)*/
/*(*1 Instructions *)*/
/*(*************************************************************************)*/

instr:
 | TARITH cond  imsr TCOMMA reg TCOMMA reg 
     { (Arith ($1, None, $3, Some $5, $7), $2) }
 | TARITH cond  imsr TCOMMA reg  { (Arith ($1,  None, $3, None, $5), $2) }
 | TMVN   cond  imsr TCOMMA reg  { (Arith (MVN, None, $3, None, $5), $2) }

 | TMOV   cond  gen  TCOMMA gen     { (MOVE ($1, None, $3, $5), $2) }

 | TSWAP  cond  reg  TCOMMA ireg    { (SWAP ($1, $5, $3, None), $2) }
 | TSWAP  cond  ireg TCOMMA reg     { (SWAP ($1, $3, $5, None), $2) }
 | TSWAP  cond  reg  TCOMMA ireg TCOMMA reg 
     { (SWAP ($1, $5, $3, Some $7), $2) }

 /*(*stricter: no cond here, use Bxx form, so normalized AST *)*/
 | TB        branch           { (B $2, AL) }
 | TBx       rel              { (Bxx ($1, $2), AL) }
 | TBL  cond branch           { (BL $3, $2)}
 | TCMP cond imsr TCOMMA reg  { (Cmp ($1, $3, $5), $2) } 

 | TSWI cond imm { (SWI $3, $2) }
 | TRFE cond     { (RFE, $2) }

/*(*************************************************************************)*/
/*(*1 Operands *)*/
/*(*************************************************************************)*/

imsr:
 | imm   { Imm $1 }
 | shift { $1 }
 | reg   { Reg $1 }


imm: TDOLLAR con      { $2 }

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


reg:
 | TRx                { $1 }
 /*(* stricter? could remove, redundant with cpp *)*/
 | TR TOPAR expr TCPAR 
     { if $3 <= 15 && $3 >= 0
       then R $3
       else error "register value out of range"
     }


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
 | ximm  { match $1 with Left x -> Imsr (Imm x) | Right x -> Ximm x }
 | shift { Imsr ($1) }
 | reg   { Imsr (Reg $1) }

 | ioreg { $1 }
 | name                    { Entity $1 }
 | con TOPAR pointer TCPAR { Entity ($3 None $1) }

ximm:
 | imm             { Left $1 }
 /*(* todo: float *)*/
 | TDOLLAR TSTRING { Right (String $2) }
 | TDOLLAR name    { Right (Address $2) }

ioreg:
 | ireg                { Indirect ($1, 0) }
 | con TOPAR reg TCPAR { Indirect ($3, $1) }

ireg: TOPAR reg TCPAR { $2 }


name: 
 | TIDENT offset         TOPAR pointer TCPAR { $4 (Some (mk_e $1 false)) $2 }
 | TIDENT TLT TGT offset TOPAR TSB     TCPAR { Global (mk_e $1 true, $4) }

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



branch: 
 | rel               { $1 }
 | global            { ref (SymbolJump $1) }
 | ireg              { ref (IndirectJump $1) }

rel:
 | TIDENT offset        { ref (LabelUse ($1, $2)) }
 | con TOPAR TPC TCPAR  { ref (Relative $1) }


/*(*************************************************************************)*/
/*(*1 Misc *)*/
/*(*************************************************************************)*/

/*(* todo: special bits or inline in previous rule? *)*/
cond:
 | /* empty */ { AL }
 | TCOND  { $1 }
