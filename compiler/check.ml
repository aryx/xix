(* Copyright 2016 Yoann Padioleau, see copyright.txt *)
open Common

open Ast

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* This module makes sure every entity used is defined. In some cases
 * it also checks if an entity is unused or incorrectly redeclared.
 * 
 * For typechecking see typecheck.ml.
 * For naming see parser.mly.
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type usedef = {
  mutable defined: Ast.loc option;
  mutable used: Ast.loc option;
}

type env = {
  ids:      (fullname, usedef * Ast.type_) Hashtbl.t;
  tags:     (fullname, usedef * Ast.tagkind) Hashtbl.t;
  typedefs: (fullname, usedef) Hashtbl.t;
  labels:   (string, usedef) Hashtbl.t;

  mutable local_entities: (local_entity list) list;
}
and local_entity =
    | Id of fullname (* parameter, variable *)
    | Label of string

(*****************************************************************************)
(* Use/Def *)
(*****************************************************************************)

(* use of undefined, redefined, unused 
 * todo:
 *  - make sure consistent use of tagkind
 *  - consistent use of type?
 *)
let check_usedef program =
  raise Todo

(* USED/SET *)

(*****************************************************************************)
(* Format checking *)
(*****************************************************************************)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let check_program ast =
  check_usedef ast

