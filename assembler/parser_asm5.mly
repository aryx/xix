%{
(* Copyright 2015, 2016 Yoann Padioleau, see copyright.txt *)
open Common

open Ast_asm5

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

let error s =
  failwith (spf "Syntax error: %s at line %d" s !Globals.line)

(* less: should use keywords in Asm5 instead of abuse integers *)
let attributes_of_int i =
   match i with 
   | 0 -> [] 
   | 1 -> [NOPROF] 
   | 2 -> [DUPOK]
   | 3 -> [DUPOK; NOPROF]
   | _ -> error (spf "unknown attribute or attribute combination: %d" i)

let mk_e name static = 
  { name = name;
    priv = if static then Some (-1) else None;
    signature = None;
  }
%}

/*(*************************************************************************)*/
/*(*1 Tokens *)*/
/*(*************************************************************************)*/

/*(*-----------------------------------------*)*/
/*(*2 opcodes *)*/
/*(*-----------------------------------------*)*/

%token <Ast_asm5.arith_opcode> TARITH
%token TMVN
%token <Ast_asm5.move_size> TMOV TSWAP
%token TB  TBL TRET
%token <Ast_asm5.cmp_opcode> TCMP   
%token <Ast_asm5.condition> TBx TCOND
%token TSWI TRFE

%token TTEXT TGLOBL 
%token TDATA TWORD 

/*(*-----------------------------------------*)*/
/*(*2 registers *)*/
/*(*-----------------------------------------*)*/

%token <Ast_asm5.register> TRx
%token TR
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
%token EOF

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
%token <int * string> TSharpLine

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
 | label_def line  { $1::$2 }

 /*(*pad: I added that, was handled via global originally *)*/
 | TSharpLine    TSEMICOLON { [(LineDirective (fst $1, snd $1), $2)] }

label_def: TIDENT TCOLON    { (LabelDef $1, !Globals.line) }

/*(*************************************************************************)*/
/*(*1 Pseudo instructions *)*/
/*(*************************************************************************)*/
/*(* can't factorize in attr_opt, shift/reduce conflict with TCOMMA *)*/
pseudo_instr:
 | TTEXT  entity TCOMMA imm    { TEXT  ($2, [], $4) }
 | TGLOBL entity TCOMMA imm    { GLOBL ($2, [], $4) }

 /*(* less: would be better to have mnemonics for attributes too *)*/
 | TTEXT entity TCOMMA con TCOMMA imm
     { TEXT ($2, attributes_of_int $4, $6) }
 | TGLOBL entity TCOMMA con TCOMMA imm
     { GLOBL ($2, attributes_of_int $4, $6) }

 | TDATA entity_and_offset TSLASH con TCOMMA ximm  
     { DATA (fst $2, snd $2, $4, $6) }

/*(* pad: I introduced this intermediate rule *)*/
entity: name
  { match $1 with
    | Entity (e, 0) -> e
    | _ -> error "entity expected"
  } 

entity_and_offset: name
  { match $1 with
    | Entity (e, n) -> (e, n)
    | _ -> error "entity with offset expected"
  } 


/*(*************************************************************************)*/
/*(*1 Instructions *)*/
/*(*************************************************************************)*/

instr:
 | TARITH cond  imsr TCOMMA reg TCOMMA reg { (Arith ($1, $3, Some $5, $7),$2) }
 | TARITH cond  imsr TCOMMA reg            { (Arith ($1, $3, None,    $5),$2) }
 | TMVN   cond  imsr TCOMMA reg            { (Arith (MVN, $3, None,   $5),$2) }

 | TMOV   cond  gen  TCOMMA gen     { (MOV ($1, $3, $5), $2) }

 | TSWAP  cond  reg  TCOMMA ireg    { (SWAP ($1, $5, $3, None), $2) }
 | TSWAP  cond  ireg TCOMMA reg     { (SWAP ($1, $3, $5, None), $2) }
 | TSWAP  cond  reg  TCOMMA ireg TCOMMA reg 
     { (SWAP ($1, $5, $3, Some $7), $2) }
 /*(*stricter: no cond here, use Bxx form, so normalized AST *)*/
 | TB        branch           { (B $2, AL) }
 | TBx       rel              { (Bxx ($1, $2), AL) }
 | TBL  cond branch           { (BL $3, $2)}
 | TCMP cond imsr TCOMMA reg  { (Cmp ($1, $3, $5), $2) } 
 | TRET cond                  { (RET, $2) }

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
 | name  { $1 }
 | con TOPAR pointer TCPAR { $3 None $1 }

ximm:
 | imm             { Left $1 }
 (* todo: float *)
 | TDOLLAR TSTRING { Right (String $2) }
 | TDOLLAR entity  { Right (Address $2) }

ioreg:
 | ireg                { Indirect ($1, 0) }
 | con TOPAR reg TCPAR { Indirect ($3, $1) }

ireg: TOPAR reg TCPAR { $2 }


name: 
 | TIDENT offset         TOPAR pointer TCPAR { $4 (Some (mk_e $1 false)) $2 }
 | TIDENT TLT TGT offset TOPAR TSB     TCPAR { Entity (mk_e $1 true, $4) }

pointer: 
 | TSB  { (fun name_opt offset ->
           match name_opt with
           | None -> error "identifier expected"
           | Some e -> Entity (e, offset)
          )
         }
 | TSP  { (fun name_opt offset ->
           match name_opt with
           | None -> Param (None, offset)
           | Some ({name = s; priv = _false}) -> Param (Some s, offset)
           )
         }
 | TFP  { (fun name_opt offset ->
           match name_opt with
           | None -> Local (None, offset)
           | Some ({name = s; priv = _false}) -> Local (Some s, offset)
           )
         }


offset:
 | /* empty */ { 0 }
 | TPLUS  con  { $2 }
 | TMINUS con  { - $2 }



branch: 
 | rel               { $1 }
 | entity_and_offset { SymbolJump (fst $1, snd $1) }
 | ireg              { IndirectJump $1 }

rel:
 | TIDENT offset        { LabelUse ($1, $2) }
 | con TOPAR TPC TCPAR  { Relative $1 }


/*(*************************************************************************)*/
/*(*1 Misc *)*/
/*(*************************************************************************)*/

/*(* TODO: special bits or inline in previous rule? *)*/
cond:
 | /* empty */ { AL }
 | TCOND  { $1 }
