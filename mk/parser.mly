%{
(* Copyright 2016 Yoann Padioleau, see copyright.txt *)
open Common

open Ast

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* 
 * todo:
 *  - good parsing error messages
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let error s =
  failwith (spf "%s:%d: Syntax error %s" !Globals.file !Globals.line s)

let contain_percent words =
  words |> List.exists (fun word ->
    word |> List.exists (fun word_elem -> word_elem = Percent)
  )

let attrs_of_string s =
  pr2 "TODO: attrs_of_string";
  []

%}

/*(*************************************************************************)*/
/*(*1 Tokens *)*/
/*(*************************************************************************)*/

%token <string> TSpace
%token TNewline

%token <Ast.loc> TColon TEq TInf
%token TPercent

%token <string> TVar TVarColon
%token <string> TQuoted TBackquoted 
%token <string> TOther TLineRecipe

%token TCBrace
%token TEndRecipe

%token EOF


/*(*************************************************************************)*/
/*(*1 Rules type declaration *)*/
/*(*************************************************************************)*/

%type <Ast.instr list> program
%start program

%%

/*(*************************************************************************)*/
/*(*1 Program *)*/
/*(*************************************************************************)*/

program: instrs EOF { $1 }

/*(*************************************************************************)*/
/*(*1 Instr *)*/
/*(*************************************************************************)*/

instrs: 
| instr instrs  { $1 @ $2 }
| /*(*empty*)*/ { [] }

instr:
 | TNewline             { [] }
 | TInf words TNewline  { [{instr = Include $2; loc = $1}] }

 /*(* stricter: no space after variable name, no private var syntax *)*/
 | TOther TEq words_opt TNewline        
     { [{instr = Definition ($1, $3); loc = $2}] }

 | words TColon words_opt TNewline recipe
     { [{instr = Rule { targets = $1; prereqs = $3; attr = []; 
                        recipe = $5;
                        is_meta = contain_percent $1;
                       };
         loc = $2;
        }]
     }
 | words TColon TOther TColon words_opt TNewline recipe 
     { [{instr = Rule { targets = $1; prereqs = $5; attr = attrs_of_string $3; 
                        recipe = $7;
                        is_meta = contain_percent $1;
                       };
         loc = $2;
        }]
     }

/*(* less: 
    "expected one of :<=\n", 
    "missing include file name\n"
    "multiple vars on left side of assignment\n"
    "unknown attribute %s" (for rule, for variable def)
    "missing trailing :" (for rule attribute)
  *)*/

/*(*************************************************************************)*/
/*(*1 Words *)*/
/*(*************************************************************************)*/

/*(* remove leading spaces *)*/
words: 
 | spaces words_ { $2 }
 | words_        { $1 }

words_:
| word spaces words_ { $1 :: $3 }
| word        { [$1] }
/*(* remove trailing spaces *)*/
| word spaces { [$1] }

/*(* stricter: forbid just spaces; if have space, then must have a word *)*/
words_opt:
| words          { $1 }
| /*(*empty*)*/  { [] }

/*(*less: normalize to concatenate possible TOther "xxx"::TOther "=" ? *)*/
word:
| word_elem word { $1::$2 }
| word_elem      { [$1] }

word_elem:
| TOther      { String $1 }
| TPercent    { Percent }
| TQuoted     { String $1 }
| TBackquoted { Backquoted $1 }
| TVar        { Var (SimpleVar $1) }
| TVarColon word TEq word TCBrace { Var (SubstVar ($1, $2, $4)) }

/*(* the lexer agglomerates spaces in one TSpace token, but then the escaped
   * newline are not agglomerated so we may have multiple TSpace *)*/
spaces: 
 | TSpace { }
 | TSpace spaces { }

/*(*************************************************************************)*/
/*(*1 recipe *)*/
/*(*************************************************************************)*/
recipe: recipe_lines_opt TEndRecipe 
 { if $1 = [] then None else Some $1 }

recipe_lines_opt:
 | /*(* empty *)*/              { [] }
 | TLineRecipe recipe_lines_opt { $1 :: $2 }

