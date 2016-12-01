(* Copyright 2016 Yoann Padioleau, see copyright.txt *)
open Common

open Ast
module T = Type
module S = Storage
module E = Check

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* This module assigns a final (resolved) type to every identifiers.
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
  structs: (Ast.fullname, Type.struct_kind * Type.structdef) Hashtbl.t;
  typedefs: (Ast.fullname, Type.t) Hashtbl.t;
  constants: (Ast.fullname, integer) Hashtbl.t;
  (* less: enum? fullname -> Type.t but only basic? *)
}

(* less: factorize things in error.ml? *)
type error = Check.error

let string_of_error err =
  Check.string_of_error err

exception Error of error

(*****************************************************************************)
(* Constant expression evaluator *)
(*****************************************************************************)


(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* will expand typedefs, resolve constant expressions *)
let rec asttype_to_type env typ0 =
  match typ0.t with
  | TBase t -> t
  | TPointer typ -> Type.TPointer (asttype_to_type env typ)
  | TArray (eopt, typ) ->
      raise Todo
  | TStructName (su, fullname) -> Type.TStructName (su, fullname)
  | TEnumName fullname ->
      raise Todo
  | TTypeName fullname -> Hashtbl.find env.typedefs fullname
  | TFunction (tret, (tparams, tdots)) ->
    Type.TFunc (asttype_to_type env tret,
                tparams |> List.map (fun p -> asttype_to_type env p.p_type),
                tdots)


(* if you declare multiple times the same global, we need to make sure
 * the types are the same. ex: 'extern int foo; ... int foo = 1;'
 * This is where we detect inconsistencies like 'int foo; void foo();'
 *)
let sametype t1 t2 =
  t1 = t2


(* if you declare multiple times the same global, we may need to merge
 * types. Really???
*)
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

  let funcs = ref [] in
  let globals = ref [] in

  let rec toplevel env = function
    | StructDef { s_kind = su; s_name = fullname; s_loc = loc; s_flds = flds }->
      Hashtbl.add env.structs fullname 
        (su, flds |> List.map 
            (fun {fld_name = name; fld_loc=_; fld_type = typ } ->
              (name, asttype_to_type env typ)))

    | TypeDef { typedef_name = fullname; typedef_loc = loc; typedef_type =typ}->
      Hashtbl.add env.typedefs fullname (asttype_to_type env typ)

    | EnumDef { e_name = fullname; e_loc = loc; e_constants = csts } ->
      raise Todo

    | VarDecl { v_name = fullname; v_loc = loc; v_type = typ;
                v_storage = stoopt; v_init = eopt} ->
      let t = asttype_to_type env typ in
      (try 
         let (oldt, oldsto, oldloc) = Hashtbl.find env.ids fullname in
         (* check type compatibility *)
         if not (sametype t oldt)
         then raise (Error (E.Inconsistent (
              (* less: could dump both type using vof_type *)
               spf "redefinition of '%s' with a different type" 
                 (unwrap fullname), loc,
               "previous definition is here", oldloc)))
         else
           (* TODO: need merge?? *)
           let finalt = t in
           (* check storage compatibility and compute final storage *)
           let finalsto = 
             match stoopt, oldsto with
             (* TODO: adjust! *)
             | _ -> Storage.Global
           in
           Hashtbl.replace env.ids fullname (finalt, finalsto, loc)
       with Not_found ->
         let finalsto =
           match stoopt with
           | _ -> Storage.Global
         in
         Hashtbl.add env.ids fullname (t, finalsto, loc)
      )

    | FuncDef { f_name = name; f_loc = loc; f_type = ftyp; f_body = st } ->
      raise Todo

  and stmt env st0 = function
    | _ -> raise Todo
  in

  let env = {
    ids = Hashtbl.create 101;
    structs = Hashtbl.create 101;
    typedefs = Hashtbl.create 101;
    constants = Hashtbl.create 101;
  }
  in
  ast |> List.iter (toplevel env);
  env, List.rev !funcs, List.rev !globals
