(* Copyright 2016 Yoann Padioleau, see copyright.txt *)

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* This module assigns the final (resolved) type to every identifiers.
 * Typedefs are expanded, struct definitions are computed.
 * 
 * Thanks to the naming done in parser.mly and the unambiguous Ast.fullname,
 * we do not have to handle scope here.
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* less: vlong? *)
type integer = int

(* Environment for typechecking *)
type env = {
  ids:  (Ast.fullname, Type.t * Storage.t) Hashtbl.t;
  tags: (Ast.fullname, Type.tagdef) Hashtbl.t;
  typedefs: (Ast.fullname, Type.t) Hashtbl.t;

  constants: (Ast.fullname, integer) Hashtbl.t;
  (* labels: string, ??  *)
}

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* if declare multiple times the same global *)
let compatible_types t1 t2 =
  raise Todo

(* if declare multiple times the same global *)
let merge_types t1 t2 =
  raise Todo


(* if declare multiple times the same global *)
let merge_storage oldstorage laststorage =
  raise Todo

(* when processing enumeration constants *)
let maxtype t1 t2 =
  raise Todo

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

(* todo:
 *  - tmerge to compare decl to def.
 *  - evaluate const_expr  "enum not a constant: %s"
 *  - stuff done by 5c at parsing time:
 *    * type of Cast
 *    * type of lexpr
 *    * type of Return
 *    * type of identifier
 *    * type of constants (integers, floats, strings)
 *  - adjust storage when have more information
 *     (also if initializer => extern to global)
 *  - check if redeclare things (which is different from refining things)
 *    for instance two parameters with same name, if two locals with same name.
 *    or if redefine same structure in same scope, or conflicting sukind
 *    for the same tag.
 *    (or do that in check.ml??)
 *  - can define enum with same name than global entity? there would
 *    have the same blockid but will get ambiguity then.
 *    (or do that in check.ml??)
 *)
