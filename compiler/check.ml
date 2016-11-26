(* Copyright 2016 Yoann Padioleau, see copyright.txt *)

open Ast
(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* For typechecking see typecheck.ml.
 * For naming see parser.mly.
 * 
 * This module makes sure every entity used is defined. In some cases
 * it also checks if an entity is unused or wrongly redeclareded.
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type usedef = {
  mutable defined: Ast.pos option;
  mutable used: Ast.pos option;
}

type env = {
  ids:      (fullname, usedef * Ast.type_)) Hashtbl.t;
  tags:     (fullname, usedef * Ast.tagkind) Hashtbl.t;
  typedefs: (fullname, usedef) Hashtbl.t;
  labels:   (string, usedef) Hashtbl.t;
}

(*****************************************************************************)
(* Use/Def *)
(*****************************************************************************)

(* use of undefined, redefined, unused 
 * todo:
 *  - make sure consistent use of takkind
 *  - consistent use of type?
 *)
let check_usedef program =
  raise Todo

(* USED/SET *)

(*****************************************************************************)
(* Format checking *)
(*****************************************************************************)

