/*(*s: Parser.mly *)*/
%{
(* Copyright 2016 Yoann Padioleau, see copyright.txt *)
open Common
open Fpath_.Operators
open Ast

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Limitations compared to mk:
 *  - backquote only in words context, not at toplevel
 *    (you can not do  `echo '<foo.c'` and expect it to include foo.c)
 * todo:
 *  - good parsing error messages, right now hard.
 *    "missing include file name\n"    < no words.
 *    "multiple vars on left side of assignment\n"  words =
 *    "missing trailing :" (for rule attribute)
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
/*(*s: function [[Parser.error_loc]] *)*/
let error_loc (loc : Ast.loc) (s : string) =
  failwith (spf "%s:%d: Syntax error, %s" !!(loc.file) loc.line s)
/*(*e: function [[Parser.error_loc]] *)*/
/*(*s: function [[Parser.error]] *)*/
let error s =
  error_loc { file = Fpath.v !Globals.file; line = !Globals.line } s
/*(*e: function [[Parser.error]] *)*/
/*(*s: function [[Parser.attrs_of_string]] *)*/
let attrs_of_string loc s =
  s |> List_.list_of_string |> List.map (function
    | 'Q' -> Quiet
    | 'V' -> Virtual
    | 'D' -> Delete
    | 'I' -> Interactive
    | ('N' | 'R' | 'n') as c  -> NotHandled c
    | c -> error_loc loc (spf "unknown attribute: %c" c)
  )
/*(*e: function [[Parser.attrs_of_string]] *)*/
%}

/*(*************************************************************************)*/
/*(*1 Tokens *)*/
/*(*************************************************************************)*/
/*(*s: Parser tokens *)*/
%token <string> TSpace
%token TNewline

%token <Ast.loc> TColon TEq TInf
%token TPercent

%token <string> TVar
%token <string> TQuoted
%token <string> TOther

%token <string> TLineRecipe
%token TEndRecipe

/*(*s: Parser extra tokens *)*/
%token <string> TBackquoted
/*(*x: Parser extra tokens *)*/
%token <string> TVarColon
%token TCBrace
/*(*x: Parser extra tokens *)*/
%token <Ast.loc> TInfPipe
/*(*e: Parser extra tokens *)*/

%token EOF
/*(*e: Parser tokens *)*/

/*(*************************************************************************)*/
/*(*1 Rules type declaration *)*/
/*(*************************************************************************)*/
/*(*s: Parser entry points types *)*/
%type <Ast.instr list> program
%start program
/*(*e: Parser entry points types *)*/
%%

/*(*s: grammar *)*/
/*(*************************************************************************)*/
/*(*1 Program *)*/
/*(*************************************************************************)*/
/*(*s: rule [[Parser.program]] *)*/
program: instrs EOF { $1 }
/*(*e: rule [[Parser.program]] *)*/

/*(*************************************************************************)*/
/*(*1 Instr *)*/
/*(*************************************************************************)*/
/*(*s: rule [[Parser.instrs]] *)*/
instrs: 
 | instr instrs  { $1 @ $2 }
 | /*(*empty*)*/ { [] }
/*(*e: rule [[Parser.instrs]] *)*/
/*(*s: rule [[Parser.instr]] *)*/
instr:
 | TInf words TNewline 
    { [{ instr = Include $2; loc = $1 }] }
 /*(* stricter: no space after variable name, no private var syntax *)*/
 | TOther TEq words_opt TNewline        
    { [{ instr = Definition ($1, $3); loc = $2 }] }
 /*(* the rule! *)*/
 | words TColon words_opt TNewline recipe
    { [{ instr = Rule { targets=$1; prereqs=$3; attrs=[]; recipe=$5;}; loc = $2; }] }
 /*(*s: rule [[Parser.instr]] other cases *)*/
 | TNewline             { [] }
 | error TNewline { error "expected one of :<=\n" }
 /*(*x: rule [[Parser.instr]] other cases *)*/
 | TInfPipe words TNewline  { [{instr = PipeInclude $2; loc = $1}] }
 /*(*x: rule [[Parser.instr]] other cases *)*/
 | words TColon TOther TColon words_opt TNewline recipe 
     { [{instr = Rule { targets = $1; prereqs = $5; 
                        attrs = attrs_of_string $2 $3; 
                        recipe = $7;
                       };
         loc = $2;
        }]
     }
 /*(*e: rule [[Parser.instr]] other cases *)*/
/*(*e: rule [[Parser.instr]] *)*/

/*(* less: 
instr: error { }
  *)*/

/*(*************************************************************************)*/
/*(*1 Words *)*/
/*(*************************************************************************)*/
/*(*s: word rules *)*/
words: 
 /*(* remove leading spaces *)*/
 | spaces words_ { $2 }
 | words_        { $1 }
/*(*x: word rules *)*/
/*(* the lexer agglomerates spaces in one TSpace token, but then the escaped
   * newline are not agglomerated so we may have multiple TSpace so
   * we should also normalize that.
   *)*/
spaces: 
 | TSpace { }
 | TSpace spaces { }
/*(*x: word rules *)*/
words_:
 | word spaces words_ { (W $1) :: $3 }
 | word        { [W $1] }
 /*(* remove trailing spaces *)*/
 | word spaces { [W $1] }
/*(*x: word rules *)*/
/*(*less: normalize to concatenate possible TOther "xxx"::TOther "=" ? *)*/
word:
 | word_elem word { ($1::$2) }
 | word_elem      { [$1] }
/*(*x: word rules *)*/
word_elem:
 | TOther      { String $1 }
 | TQuoted     { String $1 }
 | TPercent    { Percent }
 | TVar        { Var (SimpleVar $1) }
 /*(*s: rule [[Parser.word_elem]] other cases *)*/
 | TBackquoted { Backquoted $1 }
 /*(*x: rule [[Parser.word_elem]] other cases *)*/
 | TVarColon word TEq words TCBrace { Var (SubstVar ($1, W $2, $4)) }
 /*(*e: rule [[Parser.word_elem]] other cases *)*/
/*(*e: word rules *)*/

/*(*************************************************************************)*/
/*(*1 recipe *)*/
/*(*************************************************************************)*/
/*(*s: recipe rules *)*/
recipe: recipe_lines_opt TEndRecipe 
 { if $1 = [] then None else Some (R $1) }

recipe_lines_opt:
 | /*(* empty *)*/              { [] }
 | TLineRecipe recipe_lines_opt { $1 :: $2 }
/*(*e: recipe rules *)*/

/*(*************************************************************************)*/
/*(*1 EBNF rules *)*/
/*(*************************************************************************)*/
/*(*s: ebnf rules *)*/
/*(* stricter: forbid just spaces; if have space, then must have a word *)*/
words_opt:
 | words          { $1 }
 | /*(*empty*)*/  { [] }
/*(*e: ebnf rules *)*/
/*(*e: grammar *)*/
/*(*e: Parser.mly *)*/
