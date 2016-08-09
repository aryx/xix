(*s: yacc/ast.ml *)
(*s: copyright ocamlyacc *)
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
(*e: copyright ocamlyacc *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(*s: type Ast.term (yacc) *)
(* uppercase string *)
type term = T of string
(*e: type Ast.term (yacc) *)
(*s: type Ast.nonterm (yacc) *)
(* lowercase string *)
type nonterm = NT of string
(*e: type Ast.nonterm (yacc) *)

(*s: type Ast.symbol (yacc) *)
type symbol = Term of term | Nonterm of nonterm
(*e: type Ast.symbol (yacc) *)

(*s: type Ast.charpos (yacc) *)
type charpos = int
(*e: type Ast.charpos (yacc) *)
(*s: type Ast.location (yacc) *)
type location =
    Location of charpos * charpos
(*e: type Ast.location (yacc) *)
(*s: type Ast.action (yacc) *)
(* the slice may contain the special $<digit> markers *)
type action = location
(*e: type Ast.action (yacc) *)

(*s: type Ast.grammar (yacc) *)
type grammar = rule_ list
(*e: type Ast.grammar (yacc) *)
(*s: type Ast.rule_ (yacc) *)
  and rule_ = {
    lhs: nonterm;
    rhs: symbol list;
    act: action;
  }
(*e: type Ast.rule_ (yacc) *)

(*s: type Ast.directive (yacc) *)
type directive =
  | Token of type_ option * term
  | Start of nonterm
  | Type of type_ * nonterm
  | Prec of unit (* TODO *)
(*e: type Ast.directive (yacc) *)

(*s: type Ast.type_ (yacc) *)
  and type_ = string
(*e: type Ast.type_ (yacc) *)

(*s: type Ast.parser_definition (yacc) *)
(* main data structure *)
type parser_definition = {
  header: location;
  directives: directive list;
  grm: grammar;
  trailer: location;
}
(*e: type Ast.parser_definition (yacc) *)

(*s: constant Ast.noloc (yacc) *)
let noloc = Location(0, 0)
(*e: constant Ast.noloc (yacc) *)

(* for the augmented grammar *)

(*s: constant Ast.start_nonterminal (yacc) *)
(* They should not conflict with user-defined terminals or non terminals
 * because nonterminals cannot contain '$' according to lexer.mll and 
 * terminals must start with an uppercase letter according again
 * to lexer.mll
 *)
let start_nonterminal = NT "S$"
(*e: constant Ast.start_nonterminal (yacc) *)
(*s: constant Ast.dollar_terminal (yacc) *)
let dollar_terminal = T "$"
(*e: constant Ast.dollar_terminal (yacc) *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(*s: function Ast.start_symbol (yacc) *)
let start_symbol def =
  try 
    (match
      def.directives |> List.find (function
        | Start x -> true
        | _ -> false
      )
     with
     | Start x -> x
     | _ -> failwith "impossible"
    )
  with Not_found -> failwith "no start symbol found"
(*e: function Ast.start_symbol (yacc) *)
(*e: yacc/ast.ml *)
