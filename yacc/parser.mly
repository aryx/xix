/*(*s: yacc/parser.mly *)*/
%{
/*(*s: copyright ocamlyacc *)*/
(* Yoann Padioleau
 *
 * Copyright (C) 2015 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file license.txt.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * license.txt for more details.
 *)
/*(*e: copyright ocamlyacc *)*/
open Ast

%}

/*(*s: type Parser.token (yacc) *)*/
%token <Ast.term> TTerm
%token <Ast.nonterm> TNonterm
%token Ttoken Tprec Tstart Ttype
%token TColon TOr TSemicolon
%token <string> TAngle
%token <Ast.location> TAction
%token TEOF
/*(*e: type Parser.token (yacc) *)*/

/*(*s: Parser entry point definition (yacc) *)*/
%start parser_definition
%type <Ast.parser_definition> parser_definition
/*(*e: Parser entry point definition (yacc) *)*/

%%

/*(*s: grammar (yacc) *)*/
/*(*s: yacc top rule *)*/
parser_definition: header directives_opt grammar header_opt TEOF 
  { { header = $1; directives = $2; grm = $3; trailer = $4 } }
;
/*(*e: yacc top rule *)*/

/*(*s: yacc header rule *)*/
header: TAction { $1 }
;
/*(*e: yacc header rule *)*/

/*(*s: yacc directive rule *)*/
directive:
   Ttoken type_opt terms { $3 |> List.map (fun t -> Token ($2, t)) }
 | Tprec                 { [Prec ()] }
 | Tstart TNonterm       { [Start $2] }
 | Ttype TAngle TNonterm { [Type ($2, $3)] }
;
/*(*e: yacc directive rule *)*/

/*(*s: yacc grammar rule *)*/
grammar: rules_opt { $1 }
;
/*(*e: yacc grammar rule *)*/

/*(*s: yacc rule rule *)*/
rule_: TNonterm TColon cases TSemicolon 
  { $3 |> List.map (fun (case, action) -> 
    { lhs = $1; rhs = case; act = action }) 
  }
;
/*(*x: yacc rule rule *)*/
cases: 
   symbols_opt TAction           { [$1, $2] }
 | symbols_opt TAction TOr cases { ($1, $2)::$4 }
;
/*(*x: yacc rule rule *)*/
symbol: 
   TTerm    { Term $1 }
 | TNonterm { Nonterm $1 }
;
/*(*e: yacc rule rule *)*/

/*(*s: yacc extra rules *)*/
header_opt:
          { Ast.noloc }
 | header { $1 }  
;
/*(*x: yacc extra rules *)*/
rules_opt: 
                   { [] }
 | rule_ rules_opt { $1 @ $2 }
;
symbols_opt:
                      { [] }
 | symbol symbols_opt { $1::$2 }
;
/*(*x: yacc extra rules *)*/
directives_opt:
                            { [] }
 | directive directives_opt { $1 @ $2 }
;
type_opt:
          { None }
 | TAngle { Some $1 }  
;
terms: TTerm terms_opt { $1::$2 }
;

terms_opt:
                   { [] }
 | TTerm terms_opt { $1::$2 }
;
/*(*e: yacc extra rules *)*/
/*(*e: grammar (yacc) *)*/

/*(*e: yacc/parser.mly *)*/
