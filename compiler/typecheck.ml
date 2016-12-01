(* Copyright 2016 Yoann Padioleau, see copyright.txt *)
open Common

open Ast
module T = Type
module S = Storage

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* This module assigns the final (resolved) type to every identifiers.
 * Typedefs are expanded, struct definitions are computed.
 * It also assigns the final storage to every identifiers.
 * 
 * Thanks to the naming done in parser.mly and the unambiguous Ast.fullname,
 * we do not have to handle scope here. 
 * Thanks to check.ml we do not have to check for inconcistencies or
 * redefinition of tags. We can assume everything is fine.
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* less: vlong? *)
type integer = int

(* Environment for typechecking *)
type env = {
  ids:  (Ast.fullname, Type.t * Storage.t * Location_cpp.loc) Hashtbl.t;
  tags: (Ast.fullname, Type.tagdef) Hashtbl.t;
  typedefs: (Ast.fullname, Type.t) Hashtbl.t;
  constants: (Ast.fullname, integer) Hashtbl.t;
}

(* less: factorize things in error.ml? *)
type error = Check.error

let string_of_error err =
  Check.string_of_error err

exception Error of error

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let expand_typedefs env typ =
  raise Todo

(* if you declare multiple times the same global, we need to make sure
 * the types are compatible. ex: 'extern int foo; and int foo = 1;'
 * This is where we detect inconsistencies like 'int foo; void foo();'
 *)
let compatible_types t1 t2 =
  raise Todo

(* if declare multiple times the same global *)
let merge_types t1 t2 =
  raise Todo


(* if you declare multiple times the same global, we need to make sure
 * the storage declaration are compatible and we need to compute the
 * final storage.
 *)
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
 * 
 *  - check if redeclare things (which is different from refining things)
 *    for instance two parameters with same name, if two locals with same name.
 *    or if redefine same structure in same scope, or conflicting sukind
 *    for the same tag.
 *    (or do that in check.ml??)
 *  - can define enum with same name than global entity? there would
 *    have the same blockid but will get ambiguity then.
 *    (or do that in check.ml??)
 *)

let check_and_annotate_program ast =

  let rec toplevel env = function
    | StructDef { s_kind = su; s_name = fullname; s_loc = loc; s_flds = flds }->
      raise Todo
    | EnumDef { e_name = fullname; e_loc = loc; e_constants = csts } ->
      raise Todo
    | TypeDef { typedef_name = fullname; typedef_loc = loc; typedef_type =typ}->
      raise Todo
    | FuncDef { f_name = name; f_loc = loc; f_type = ftyp; f_body = st } ->
      raise Todo
    | VarDecl { v_name = fullname; v_loc = loc; v_type = t; v_init = eopt} ->
      raise Todo

  and stmt env st0 = function
    | _ -> raise Todo
  in

  let env = {
    ids = Hashtbl.create 101;
    tags = Hashtbl.create 101;
    typedefs = Hashtbl.create 101;
    constants = Hashtbl.create 101;
  }
  in
  
  raise Todo
