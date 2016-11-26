(* Copyright 2016 Yoann Padioleau, see copyright.txt *)

open Ast
(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* For typechecking see typecheck.ml.
 * For naming see parser.mly.
 * 
 * This module makes sure every entity used is defined. In some cases
 * it also checks if an entity is unused or incorrectly redeclared.
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type usedef = {
  mutable defined: Ast.loc option;
  mutable used: Ast.loc option;
}

type env = {
  ids:      (fullname, usedef * Ast.type_)) Hashtbl.t;
  tags:     (fullname, usedef * Ast.tagkind) Hashtbl.t;
  typedefs: (fullname, usedef) Hashtbl.t;
  labels:   (string, usedef) Hashtbl.t;
}

(*
type local_entity = 
  | Id of fullname
  | Label of string

mutable entities_scope = (local_entity list) list;
*)

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

