(* Copyright 2016 Yoann Padioleau, see copyright.txt *)
open Common

open Ast

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* This module makes sure every entity used is defined. In some cases
 * it also checks if an entity is unused, or if it is incorrectly redeclared.
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

  (* to reset after each function, function scope *)
  mutable labels:   (string, usedef) Hashtbl.t;
  (* block scope *)
  mutable local_ids: (fullname list) list;
}


exception Error2 of 
    string * loc * (* error here *) 
    string * loc   (* previous decl/def/whatever here *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let inconsistent_tag fullname loc usedef =
  let locbefore = 
    match usedef with
    | { defined = Some loc } -> loc
    | { used = Some loc } -> loc
    | _ -> raise (Impossible "must have a def or a use")
  in
  raise (Error2 (
    spf "use of '%s' with tag type that does not match previous declaration "
      (unwrap fullname), loc,
    "previous use is here", locbefore
  ))

let check_inconsistent_or_redefined_tag env fullname tagkind loc =
  try 
    let (usedef, oldtagkind) = Hashtbl.find env.tags fullname in
    if tagkind <> oldtagkind
    then inconsistent_tag fullname loc usedef;
    (* the tag may not be defined, as in a previous 'struct Foo x;' *)
    usedef.defined |> Common.if_some (fun locdef ->
      raise (Error2 (spf "redefinition of '%s'" (unwrap fullname), loc,
                     "previous definition is here", locdef))
    );
    usedef.defined <- Some loc;
  with Not_found ->
    Hashtbl.add env.tags fullname ({defined = Some loc; used = None;}, tagkind)


let inconsistent_id fullname loc usedef =
  let locbefore = 
    match usedef with
    | { defined = Some loc } -> loc
    | { defined = None } -> raise (Impossible "id always defined first")
  in
  raise (Error2 (
    spf "redefinition of '%s' " (unwrap fullname), loc,
    "previous definition is here", locbefore
  ))


let check_inconsistent_or_redefined_id env fullname idkind loc =
  try 
    let (usedef, oldidkind) = Hashtbl.find env.ids fullname in
    if idkind <> oldidkind
    then inconsistent_id fullname loc usedef
    else
      (match usedef.defined with
      | Some locdef ->
        (* stricter: 5c allows at least for same typedef *)
        raise (Error2 (spf "redefinition of '%s'" (unwrap fullname), loc,
                       "previous definition is here", locdef))
      (* the id must be defined, there is no forward use of ids
       * (enum constants, typedefs, variables)
       *)
      | None -> raise (Impossible "ids are always defined first")
      )
  with Not_found ->
    Hashtbl.add env.ids fullname ({defined = Some loc; used = None; }, idkind)



(*****************************************************************************)
(* Use/Def *)
(*****************************************************************************)

(* use of undefined, redefined, redeclared, unused, inconsistent tags, etc. *)
let check_usedef program =

  let rec toplevel env = function
    | StructDef { s_kind = su; s_name = fullname; s_loc = loc; s_flds = flds }->
      (* checking the tag *)

      let tagkind = Ast.tagkind_of_su su in
      check_inconsistent_or_redefined_tag env fullname tagkind loc;

      (* checking the fields *)

      let hflds = Hashtbl.create 11 in
      flds |> List.iter 
       (fun {fld_name = nameopt; fld_loc = loc; fld_type = typ} ->
        nameopt |> Common.if_some (fun name ->
          (* stricter: 5c does not report, clang does *)
          if Hashtbl.mem hflds name
          then raise (Error2 (spf "duplicate member '%s'" name, loc,
                              "previous declaration is here", 
                              Hashtbl.find hflds name));
          Hashtbl.add hflds name loc;
        );
        type_ env typ
      )

    | EnumDef { e_name = fullname; e_loc = loc; e_constants = csts } ->
      (* checking the tag *)

      let tagkind = TagEnum in
      check_inconsistent_or_redefined_tag env fullname tagkind loc;

      (* checking the constants *)

      csts |> List.iter 
          (fun { ecst_name = fullname; ecst_loc = loc; ecst_value = eopt } ->
            check_inconsistent_or_redefined_id env fullname IdEnumConstant loc;
            eopt |> Common.if_some (expr env)
          );

    | TypeDef { typedef_name = fullname; typedef_loc = loc; typedef_type =typ}->
      check_inconsistent_or_redefined_id env fullname IdTypedef loc;
      type_ env typ
           
    | FuncDef def -> raise Todo
    | VarDecl { v_name = fullname; v_loc = loc; v_type = t; v_init = eopt} ->
      (if Hashtbl.mem env.ids fullname &&
          snd (Hashtbl.find env.ids fullname) = IdIdent
      (* this can be ok, you can redeclare toplevel identifiers as you
       * can give a final storage. It depends on the situation, 
       * see typecheck.ml
       *)
       then ()
       else check_inconsistent_or_redefined_id env fullname IdIdent loc
      );
      type_ env t;
      eopt |> Common.if_some (expr env)

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
        (* forward decl *)
        Hashtbl.add env.tags fullname 
          ({defined = None; used = Some typ.t_loc; }, tagkind)
      )
    | TTypeName fullname ->
        (try 
           let (usedef, idkind) = Hashtbl.find env.ids fullname in
           if idkind <> IdTypedef
           then raise (Impossible "typename returned only if typedef in scope");
           usedef.used <- Some typ.t_loc
         with Not_found ->
           raise (Impossible "typename returned only if typedef in scope")
        )
  in
  
  let env = {
    ids = Hashtbl.create 101;
    tags = Hashtbl.create 101;
    labels = Hashtbl.create 101;
    local_ids = [[]];
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

