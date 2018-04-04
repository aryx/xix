%{
(* Copyright 2016 Yoann Padioleau, see copyright.txt *)
open Common

open Ast

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Limitations compared to mk:
 *  - backquote only in words context, not at toplevel
 *    (you can not do  `echo '<foo.c'` and expect it to include foo.c)
 * todo:
 *  - good parsing error messages, right now hard.
      "missing include file name\n"    < no words.
      "multiple vars on left side of assignment\n"  words =
      "missing trailing :" (for rule attribute)
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let error_loc loc s =
  failwith (spf "%s:%d: Syntax error, %s" loc.file loc.line s)

let error s =
  error_loc { file = !Globals.file; line = !Globals.line } s


let attrs_of_string loc s =
  s |> Common2.list_of_string |> List.map (function
    | 'Q' -> Quiet
    | 'V' -> Virtual
    | 'D' -> Delete
    | 'I' -> Interactive
    | ('N' | 'R' | 'n') as c  -> NotHandled c
    | c -> error_loc loc (spf "unknown attribute: %c" c)
  )

%}

/*(*************************************************************************)*/
/*(*1 Tokens *)*/
/*(*************************************************************************)*/

%token <string> TSpace
%token TNewline

%token <Ast.loc> TColon TEq TInf
%token TPercent

%token <string> TVar TVarColon
%token <string> TQuoted TBackquoted TLineRecipe
%token <string> TOther

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
     { [{instr = Rule { targets = $1; prereqs = $3; attrs = []; recipe = $5;};
         loc = $2;
        }]
     }
 | words TColon TOther TColon words_opt TNewline recipe 
     { [{instr = Rule { targets = $1; prereqs = $5; 
                        attrs = attrs_of_string $2 $3; 
                        recipe = $7;
                       };
         loc = $2;
        }]
     }

 | error TNewline { error "expected one of :<=\n" }
 

/*(* less: 
instr: error { }
  *)*/

/*(*************************************************************************)*/
/*(*1 Words *)*/
/*(*************************************************************************)*/

/*(* remove leading spaces *)*/
words: 
 | spaces words_ { $2 }
 | words_        { $1 }

words_:
| word spaces words_ { (W $1) :: $3 }
| word        { [W $1] }
/*(* remove trailing spaces *)*/
| word spaces { [W $1] }

/*(* stricter: forbid just spaces; if have space, then must have a word *)*/
words_opt:
| words          { $1 }
| /*(*empty*)*/  { [] }

/*(*less: normalize to concatenate possible TOther "xxx"::TOther "=" ? *)*/
word:
| word_elem word { ($1::$2) }
| word_elem      { [$1] }

word_elem:
| TOther      { String $1 }
| TPercent    { Percent }
| TQuoted     { String $1 }
| TBackquoted { Backquoted $1 }
| TVar        { Var (SimpleVar $1) }
| TVarColon word TEq words TCBrace { Var (SubstVar ($1, W $2, $4)) }

/*(* the lexer agglomerates spaces in one TSpace token, but then the escaped
   * newline are not agglomerated so we may have multiple TSpace so
   * we should also normalize that.
   *)*/
spaces: 
 | TSpace { }
 | TSpace spaces { }

/*(*************************************************************************)*/
/*(*1 recipe *)*/
/*(*************************************************************************)*/
recipe: recipe_lines_opt TEndRecipe 
 { if $1 = [] then None else Some (R $1) }

recipe_lines_opt:
 | /*(* empty *)*/              { [] }
 | TLineRecipe recipe_lines_opt { $1 :: $2 }
