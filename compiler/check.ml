(* Copyright 2016 Yoann Padioleau, see copyright.txt *)
open Common

open Ast

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* This module makes sure every entity used is defined. In some cases
 * it also checks if an entity is unused, and also if it is incorrectly
 * redeclared.
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
  ids:      (fullname, usedef * Ast.idkind) Hashtbl.t;
  tags:     (fullname, usedef * Ast.tagkind) Hashtbl.t;
  typedefs: (fullname, usedef) Hashtbl.t;
  labels:   (string, usedef) Hashtbl.t;

  (* block scope *)
  mutable local_ids: (fullname list) list;
  (* function scope *)
  mutable local_labels: string list;
}


exception Error2 of 
    string * loc * (* error here *) 
    string * loc   (* previous decl/def/whatever here *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let unwrap (name, _) = name    


let inconsistent_tag (name, _block) loc usedef =
  let locbefore = 
    match usedef with
    | { defined = Some loc } -> loc
    | { used = Some loc } -> loc
    | _ -> raise (Impossible "must have a def or a use")
  in
  raise (Error2 (
    spf "use of '%s' with tag type that does not match previous declaration "
      name, loc,
    "previous use is here", locbefore
  ))


(*****************************************************************************)
(* Use/Def *)
(*****************************************************************************)

(* use of undefined, redefined, redeclared, unused, inconsistent tags, etc. *)
let check_usedef program =

  let rec toplevel env = function
    | StructDef def ->
      let tagkind = Ast.tagkind_of_su def.s_kind in
      (try 
        let (usedef, oldtagkind) = Hashtbl.find env.tags def.s_name in
        if tagkind <> oldtagkind
        then inconsistent_tag def.s_name def.s_loc usedef;
        usedef.defined |> Common.if_some (fun locdef ->
          raise (Error2 (spf "redefinition of '%s'" (unwrap def.s_name),
                         def.s_loc,
                         "previous definition is here", locdef))
        );
        
      with Not_found ->
        Hashtbl.add env.tags def.s_name 
          ({defined = Some def.s_loc; used = None; }, tagkind)
      );

      let hflds = Hashtbl.create 11 in
      def.s_flds |> List.iter (fun fld ->
        fld.fld_name |> Common.if_some (fun name ->
          (* stricter: 5c does not report, clang does *)
          if Hashtbl.mem hflds name
          then raise (Error2 (spf "duplicate member '%s'" name, fld.fld_loc,
                              "previous declaration is here", 
                              Hashtbl.find hflds name));
          Hashtbl.add hflds name fld.fld_loc;
        );
        type_ env fld.fld_type
      )

    | EnumDef def ->
      let tagkind = TagEnum in
      (try 
        let (usedef, oldtagkind) = Hashtbl.find env.tags def.e_name in
        if tagkind <> oldtagkind
        then inconsistent_tag def.e_name def.e_loc usedef;
        usedef.defined |> Common.if_some (fun locdef ->
          raise (Error2 (spf "redefinition of '%s'" (unwrap name0, def.e_loc,
                         "previous definition is here", locdef))
        );
      with Not_found ->
        Hashtbl.add env.tags def.e_name 
          ({defined = Some def.e_loc; used = None; }, tagkind)
      );


    | TypeDef def -> raise Todo
    | FuncDef def -> raise Todo
    | VarDecl decl ->
      (* todo: decl.v_name *)
      type_ env decl.v_type;
      decl.v_init |> Common.if_some (expr env)
  and stmt env = function
    | _ -> raise Todo
  and expr env = function
    | _ -> raise Todo
  and type_ env = fun typ ->
    match typ.t with
    | TBase _ -> ()
    | TPointer t -> type_ env t
    | TArray (eopt, t) -> 
      eopt |> Common.if_some (expr env);
      type_ env t;
    | TFunction (tret, (params, _dots)) ->
      type_ env tret;
      params |> List.iter (fun p ->
        (* nothing to do with p.p_name here *)
        type_ env p.p_type
      )
    | TStructName (_, fullname) | TEnumName fullname ->
      let tagkind = 
        match typ.t with
        | TStructName (su, _) -> Ast.tagkind_of_su su
        | TEnumName _ -> TagEnum
        | _ -> raise (Impossible "see pattern above")
      in
      (try 
        let (usedef, oldtagkind) = Hashtbl.find env.tags fullname in
        if tagkind <> oldtagkind
        then inconsistent_tag fullname typ.t_loc usedef;
        usedef.used <- Some typ.t_loc;
      with Not_found ->
        Hashtbl.add env.tags fullname 
          ({defined = None; used = Some typ.t_loc; }, tagkind)
      )

    | TTypeName fullname ->
        (* todo: add, Not_found impossible *)
      ()
      
  in
  
  let env = {
    ids = Hashtbl.create 101;
    tags = Hashtbl.create 101;
    typedefs = Hashtbl.create 101;
    labels = Hashtbl.create 101;
    local_entities = [[]];
  }
  in
  program |> List.iter (toplevel env);

  (* stricter: check if used but not defined tags (5c does not, clang does) *)
  (* todo: *)

  (* less: could check unused static var decl? *)
  ()

(* USED/SET *)

(*****************************************************************************)
(* Format checking *)
(*****************************************************************************)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let check_program ast =
  check_usedef ast

