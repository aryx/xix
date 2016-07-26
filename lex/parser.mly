/*(*s: lex/parser.mly *)*/
/*(*s: copyright ocamllex bis *)*/
/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  Automatique.  Distributed only by permission.                      */
/*                                                                     */
/***********************************************************************/
/*(*e: copyright ocamllex bis *)*/

/* The grammar for lexer definitions */

%{
open Ast

(* Auxiliaries for the parser. *)
/*(*s: parser helper functions and globals *)*/
/*(*x: parser helper functions and globals *)*/
/*(*x: parser helper functions and globals *)*/
let rec subtract l1 l2 =
  match l1 with
    [] -> []
  | a::r -> if List.mem a l2 then subtract r l2 else a :: subtract r l2
/*(*x: parser helper functions and globals *)*/
let named_regexps =
  (Hashtbl.create 13 : (string, regular_expression) Hashtbl.t)
/*(*x: parser helper functions and globals *)*/
let regexp_for_string s =
  let rec re_string n =
    if n >= String.length s then Epsilon
    else if succ n = String.length s then Characters([Char.code (s.[n])])
    else Sequence(Characters([Char.code (s.[n])]), re_string (succ n))
  in re_string 0
/*(*e: parser helper functions and globals *)*/

%}

/* Tokens */

/*(*s: type Parser.token *)*/
%token Trule Tparse Tand
%token <int> Tchar
%token <string> Tstring
%token Tstar Tmaybe Tplus Tor Tlparen Trparen 
%token Tlbracket Trbracket Tcaret Tdash 
%token Tunderscore Teof
%token <Ast.location> Taction
%token Tlet Tequal 
%token <string> Tident
%token Tend  
/*(*e: type Parser.token *)*/

/* Precedences and associativities. Lower precedences come first. */

/*(*s: Parser precedences and associativities *)*/
%left Tor
%left CONCAT
%nonassoc Tmaybe
%left Tstar
%left Tplus
/*(*e: Parser precedences and associativities *)*/

/* Entry points */

/*(*s: Parser entry points types *)*/
%start lexer_definition
%type <Ast.lexer_definition> lexer_definition
/*(*e: Parser entry points types *)*/

%%

/*(*s: grammar (lex) *)*/
/*(*s: lex top rule *)*/
lexer_definition:
    header named_regexps Trule definition other_definitions header Tend
        { { header = $1;
            entrypoints = $4 :: List.rev $5;
            trailer = $6
           } 
         }
;
/*(*e: lex top rule *)*/

/*(*s: lex header rule *)*/
header:
    Taction      { $1 }
  | /*epsilon*/  { Location(0,0) }
;
/*(*e: lex header rule *)*/

/*(*s: lex named regexp rule *)*/
named_regexps:
    named_regexps Tlet Tident Tequal regexp
        { Hashtbl.add named_regexps $3 $5 }
  | /*epsilon*/
        { () }
;
/*(*e: lex named regexp rule *)*/

/*(*s: lex rule rule *)*/
definition:
    Tident Tequal entry
        { ($1,$3) }
;
entry:
    Tparse case rest_of_entry
        { $2::List.rev $3 }
  | Tparse rest_of_entry
        { List.rev $2 }
;
case:
    regexp Taction
        { ($1,$2) }
;


other_definitions:
    other_definitions Tand definition
        { $3::$1 }
  | /*epsilon*/
        { [] }
;
rest_of_entry:
    rest_of_entry Tor case
        { $3::$1 }
  | /*epsilon*/
        { [] }
;
/*(*e: lex rule rule *)*/

/*(*s: lex regexp rule *)*/
regexp:
    Tchar
        { Characters [$1] }
  | regexp Tstar
        { Repetition $1 }
  | regexp Tor regexp
        { Alternative($1,$3) }
  | regexp regexp %prec CONCAT
        { Sequence($1,$2) }
  | Tlparen regexp Trparen
        { $2 }
  /*(*s: rule regexp cases *)*/
    | regexp Tmaybe
          { Alternative($1, Epsilon) }
    | regexp Tplus
          { Sequence($1, Repetition $1) }
  /*(*x: rule regexp cases *)*/
    | Tlbracket char_class Trbracket
          { Characters $2 }
  /*(*x: rule regexp cases *)*/
    | Tident
          { try
              Hashtbl.find named_regexps $1
            with Not_found ->
              prerr_string "Reference to unbound regexp name `";
              prerr_string $1;
              prerr_string "' at char ";
              prerr_int (Parsing.symbol_start());
              prerr_newline();
              exit 2 }
  /*(*x: rule regexp cases *)*/
    | Tstring
          { regexp_for_string $1 }
  /*(*x: rule regexp cases *)*/
    | Tunderscore
          { Characters all_chars }
    | Teof
          { Characters [Ast.char_eof] }
  /*(*e: rule regexp cases *)*/
;
/*(*x: lex regexp rule *)*/
char_class:
    Tcaret char_class1
        { subtract all_chars $2 }
  | char_class1
        { $1 }
;
char_class1:
    Tchar Tdash Tchar
        { char_class $1 $3 }
  | Tchar
        { [$1] }
  | char_class1 char_class1 %prec CONCAT
        { $1 @ $2 }
;

/*(*e: lex regexp rule *)*/

/*(*e: grammar (lex) *)*/

%%
/*(*e: lex/parser.mly *)*/
