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

%}

/*(*************************************************************************)*/
/*(*1 Tokens *)*/
/*(*************************************************************************)*/

/*(*-----------------------------------------*)*/
/*(*2 opcodes *)*/
/*(*-----------------------------------------*)*/

%token <Ast_asm6.width * Ast_asm6.arith_opcode> TARITH
%token <Ast_asm6.width> TCMP
%token <Ast_asm6.width> TMOV
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
%token <Ast_asm.floatp_precision> TCVTINTTOF
%token <Ast_asm.floatp_precision> TCVTFTOINT

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

 /*(* goken's ymovq/ymovl-shaped move: covers register/memory/immediate
    * in every combination MOVQ/MOVL actually need -- see Ast_asm6.ml's
    * Move comment. *)*/
 | TMOV lgen TC gen              { Move ($1, $2, $4) }

 /*(* goken's Zaut_r "built-in LEAQ" -- address-of-global only (see
    * Ast_asm6.ml's Lea comment). *)*/
 | TLEA global_and_offset TC reg { Lea (fst $2, snd $2, $4) }

 /*(* direct near call/jump, goken's ycall/yjmp's 0xe8/0xe9 rel32 forms
    * (the indirect-through-register/memory forms aren't wired). *)*/
 | TCALL branch                  { Call $2 }
 | TJMP branch                   { Jmp $2 }
 /*(* goken's yjcond-shaped conditional jump -- always to a label
    * (goken's own Ybr class), never register-indirect. *)*/
 | TJcc rel                      { Jcc ($1, $2) }

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
 | TCVTINTTOF gen TC xreg        { CvtIntToF ($1, $2, $4) }
 | TCVTFTOINT xgen TC reg        { CvtFToInt ($1, $2, $4) }

/*(*************************************************************************)*/
/*(*1 Operands *)*/
/*(*************************************************************************)*/

imr:
 | imm   { Imm $1 }
 | reg   { Reg $1 }

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
   * threads the needed REX.R/.B bits through). BP isn't wired yet
   * (would just need its own TIDENT mapping, same shape as AX/CX/../
   * DI below in Parse_asm6.ml). *)*/
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
gen:
 | reg                 { GReg $1 }
 | con TOPAR reg TCPAR { Indirect ($3, $1) }
 | name                { Entity $1 }

/*(* claude: XMM register-or-memory operand -- same addressing modes as
   * `gen` above (memory is still addressed through an ordinary GP
   * `reg`, e.g. "-8(SP)"; only the *register* alternative differs) --
   * see Ast_asm6.ml's `xgen` comment for why this isn't just `gen`
   * with an extra case. *)*/
xreg:
 | TXx { $1 }

xgen:
 | xreg                { XReg $1 }
 | con TOPAR reg TCPAR { XIndirect ($3, $1) }
 | name                { XEntity $1 }

ximm:
 | imm             { Int $1 }
 | fcon            { Float $1 }
 | TDOLLAR TSTRING { String $2 }
 | TDOLLAR name    { Address $2 }

lgen:
 | gen  { Left $1 }
 | ximm { Right $1 }

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
