%{
(* Copyright 2016 Yoann Padioleau, see copyright.txt *)
open Common

open Ast

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* 
 * todo:
 *  - good error messages
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
%token <string> TVar TQuoted TBackquoted TOther TLineRecipe
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
 |                                  TNewline  { [] }
 | TInf words                       TNewline  
     { [{instr = Include $2; loc = $1}] }

 | TOther TEq            words_opt  TNewline        
     { [{instr = Definition ($1, false, $3); loc = $2}] }
 | TOther TEq TOther TEq words_opt  TNewline        
     { raise Todo }

 | words TColon               words TNewline recipe 
     { [{instr = Rule { targets = $1; prereqs = $3; attr = []; 
                        recipe = $5;
                        is_meta = contain_percent $1;
                       };
         loc = $2;
        }]
     }
 | words TColon TOther TColon words TNewline recipe 
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

/*(* stricter: disallow leading and trailing spaces? *)*/
words: 
 | spaces words2 spaces { $2 }
 | spaces words2        { $2 }
 |        words2 spaces { $1 }
 |        words2        { $1 }

words2:
| word spaces words2  { $1 :: $3 }
| word        { [$1] }

words_opt:
| words          { $1 }
| /*(*empty*)*/  { [] }

word:
| word_elem word { $1::$2 }
| word_elem      { [$1] }

word_elem:
| TOther   { String $1 }
| TPercent { Percent}
| TQuoted  { Quoted $1 }
| TVar     { Var (SimpleVar $1) }
| TBackquoted { Backquoted $1 }

/*(* the lexer agglomerate spaces in one TSpace token, but then the escaped
   * newline are not agglomerated *)*/
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
 

/*(*************************************************************************)*/
/*(*1 Misc *)*/
/*(*************************************************************************)*/

