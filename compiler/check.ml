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
 * For naming and scope resolving see parser.mly.
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

  (* to reset after each function (because labels have a function scope) *)
  mutable labels:   (string, usedef) Hashtbl.t;
  (* block scope *)
  mutable local_ids: fullname list;
}

type error = 
  | Inconsistent of 
      string * Location_cpp.loc * (* error here *) 
      string * Location_cpp.loc   (* previous decl/def/whatever here *)
  | ErrorMisc of string * Location_cpp.loc
  | Warning of string * Location_cpp.loc

let string_of_error err =
  match err with
  | Inconsistent (s1, loc1, s2, loc2) ->
    let (file1, line1) = Location_cpp.final_loc_of_loc loc1 in
    let (file2, line2) = Location_cpp.final_loc_of_loc loc2 in
    spf "%s:%d error: %s\n%s:%d note: %s" file1 line1 s1 file2 line2 s2

  | Warning (s, loc) ->
    let (file, line) = Location_cpp.final_loc_of_loc loc in
    spf "%s:%d warning: %s" file line s
  | ErrorMisc (s, loc) ->
    let (file, line) = Location_cpp.final_loc_of_loc loc in
    spf "%s:%d error: %s" file line s

exception Error of error

let failhard = ref false

let error err =
  if !failhard
  then raise (Error err)
  else pr2 (string_of_error err)

let warn s loc =
  let err = Warning (s, loc) in
  if !failhard
  then raise (Error err)
  else pr2 (string_of_error err)
  
 
(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let inconsistent_tag fullname loc usedef =
  let locbefore = 
    match usedef with
    | { defined = Some loc } | { used = Some loc } -> loc
    | _ -> raise (Impossible "must have a def or a use")
  in
  error (Inconsistent (
    spf "use of '%s' with tag type that does not match previous declaration "
      (unwrap fullname), loc,
    "previous use is here", locbefore
  ))

let check_inconsistent_or_redefined_tag env fullname tagkind loc =
  try 
    let (usedef, oldtagkind) = Hashtbl.find env.tags fullname in
    if tagkind <> oldtagkind
    then inconsistent_tag fullname loc usedef;
    (* the tag may not have be defined, as in a previous 'struct Foo x;' *)
    usedef.defined |> Common.if_some (fun locdef ->
      error (Inconsistent (spf "redefinition of '%s'" (unwrap fullname), loc,
                     "previous definition is here", locdef))
    );
    (* now it's defined *)
    usedef.defined <- Some loc;
  with Not_found ->
    Hashtbl.add env.tags fullname ({defined = Some loc; used = None;}, tagkind)


let inconsistent_id fullname loc usedef =
  let locbefore = 
    match usedef with
    | { defined = Some loc } -> loc
    | { defined = None } -> raise (Impossible "id always defined first")
  in
  error (Inconsistent (
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
        (* stricter: 5c allows at least for same typedef; I do not. *)
        error (Inconsistent (spf "redefinition of '%s'" (unwrap fullname), loc,
                             "previous definition is here", locdef))
      (* the id must be defined, there is no forward use of ids
       * (enum constants, typedefs, variables)
       *)
      | None -> raise (Impossible "ids are always defined before being used")
      )
  with Not_found ->
    Hashtbl.add env.ids fullname ({defined = Some loc; used = None; }, idkind)

let check_unused_locals env =
  (* less: could also delete entries in env.ids *)
  env.local_ids |> List.iter (fun fullname ->
    let (usedef, idkind) = Hashtbl.find env.ids fullname in
    assert (idkind = IdIdent);
    match usedef with
    | { defined = Some loc; used = None } ->
        (* 5c says whether 'auto' or 'param' *) 
        warn (spf "variable declared and not used: '%s'" (unwrap fullname)) loc
    | { defined = None; } -> raise (Impossible "locals are always defined")
    | { defined = _; used = Some _ } -> ()
  )


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
          then error (Inconsistent (spf "duplicate member '%s'" name, loc,
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

    (* todo: if use struct tags params, they must be complete at this point *)
    | FuncDef { f_name = name; f_loc = loc; f_type = ftyp; f_body = st } ->
      let fullname = name, 0 in
      (if Hashtbl.mem env.ids fullname &&
          snd (Hashtbl.find env.ids fullname) = IdIdent
      (* this can be ok, you can redeclare toplevel identifiers as you
       * can give a final storage. It depends on the situation, 
       * see typecheck.ml
       *)
       then ()
       else check_inconsistent_or_redefined_id env fullname IdIdent loc
      );
      type_ env {t = TFunction ftyp; t_loc = loc };
      (* new function scope *)
      let env = { env with local_ids = []; labels = Hashtbl.create 11 } in
      let (_tret, (tparams, _dots)) = ftyp in
      tparams |> List.iter (fun { p_name = fullnameopt; p_loc=loc;} ->
        fullnameopt |> Common.if_some (fun fullname ->
          check_inconsistent_or_redefined_id env fullname IdIdent loc;
          env.local_ids <- fullname :: env.local_ids;
        );
      );

      (* We could match st to a Block and avoid new scope for it, but
       * it does not matter here. We do it correctly in parser.mly so
       * a parameter and local with the same name will have the same
       * blockid so we will detect if you redefine an entity even
       * if in different scope here.
       *)
      stmt env st;

      (* check function scope *)
      check_unused_locals env;
      env.labels |> Hashtbl.iter (fun name usedef ->
        match usedef with
        | { defined = Some _; used = Some _ } -> ()
        | { used = Some loc; defined = None } ->
            error (ErrorMisc (spf "use of undeclared label '%s'" name, loc))
        | { defined = Some loc; used = None } ->
            warn (spf "label declared and not used '%s'" name) loc
        | { defined = None; used = None } -> 
          raise (Impossible "at least one of used or defined")
      );
        
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

  and stmt env st0 =
    match st0.st with
    | ExprSt e -> expr env e
    | Block xs ->
        (* new block scope *)
        let env = { env with local_ids = [] } in
        List.iter (stmt env) xs;
        (* check block scope *)
        check_unused_locals env
    | If (e, st1, st2) ->
        expr env e;
        stmt env st1;
        stmt env st2;
    | While (e, st) 
    | Switch (e, st) | Case (e, st) ->
        expr env e;
        stmt env st;

    | DoWhile (st, e) ->
        stmt env st;
        expr env e
    | For (e1either, e2opt, e3opt, st) ->
        (* new block scope again *)
        let env = { env with local_ids = [] } in
        (match e1either with
        | Left e1opt -> e1opt |> Common.if_some (expr env)
        | Right decls ->
            decls |> List.iter (fun decl -> 
              stmt env ({st = Var decl; stmt_loc = decl.v_loc }) 
            )
        );
        e2opt |> Common.if_some (expr env);
        e3opt |> Common.if_some (expr env);
        stmt env st;
        (* check block scope *)
        check_unused_locals env

    | Return eopt -> eopt |> Common.if_some (expr env)
    | Continue | Break -> ()

    | Label (name, st) ->
        (try
           let usedef = Hashtbl.find env.labels name in
           usedef.defined |> Common.if_some (fun locprev ->
             error (Inconsistent (spf "redefinition of label '%s'" name, 
                                  st0.stmt_loc,
                                  "previous definition is here", locprev))
           );
           usedef.defined <- Some st0.stmt_loc;
         with Not_found ->
           Hashtbl.add env.labels name 
             {defined = Some st0.stmt_loc; used = None}
        );
        stmt env st;
    | Goto name ->
        (try 
           let usedef = Hashtbl.find env.labels name in
           usedef.used <- Some st0.stmt_loc
         with Not_found ->
           Hashtbl.add env.labels name
             { defined = None; used = Some st0.stmt_loc }
        )
    | Default st -> stmt env st

    | Var { v_name = fullname; v_loc = loc; v_type = typ; v_init = eopt } ->
      (* less: before adding in environment? can have recursive use? *)
      eopt |> Common.if_some (expr env);
    (* todo: if local VarDEcl, can actually have stuff nested like
     *  extern int i;  in which case we must go back to global
     *  scope for i! so rewrite AST? or just in typecheck.ml
     *  generate right storage for it.
     *)
      check_inconsistent_or_redefined_id env fullname IdIdent loc;
      env.local_ids <- fullname :: env.local_ids;
      type_ env typ;

  and expr env e0 =
    match e0.e with
    | Int _ | Float _ | String _ -> ()
    | Id fullname ->
        (try 
           let (usedef, _idkind) = Hashtbl.find env.ids fullname in
           usedef.used <- Some e0.e_loc
         with Not_found ->
           raise (Impossible "ids are always declared first")
        )
    | Call (e, es) -> exprs env (e::es)
    | Assign (_, e1, e2) | Binary (e1, _, e2) | Sequence (e1, e2) 
    | ArrayAccess (e1, e2) -> 
      exprs env [e1; e2]
    | RecordAccess (e, _) | RecordPtAccess (e, _) 
    | Postfix (e, _) | Prefix (_, e) | Unary (_, e) ->
      expr env e
    | Cast (typ, e) | GccConstructor (typ, e) ->
      type_ env typ;
      expr env e
    | CondExpr (e1, e2, e3) -> exprs env [e1; e2; e3]
    | SizeOf either ->
        (match either with
        | Left e -> expr env e
        | Right t -> type_ env t
        )
    | ArrayInit xs -> 
      xs |> List.iter (fun (eopt, e) -> 
        eopt |> Common.if_some (expr env);
        expr env e
      )
    | RecordInit xs -> xs |> List.iter (fun (_, e) -> expr env e)

  and exprs env xs = xs |> List.iter (expr env)

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
    local_ids = [];
  }
  in
  program |> List.iter (toplevel env);

  (* stricter: check if used but not defined tags (5c does not, clang does) *)
  env.tags |> Hashtbl.iter (fun fullname (usedef, idkind) ->
    match usedef with
    | { used = Some loc; defined = None } ->
      error (ErrorMisc (spf "use of tag '%s' that is never completed"
                          (unwrap fullname), loc))
    (* this can happen, header file are big and can cover multiple modules *)
    | { defined = Some _; used = None } -> ()
    (* perfect *)
    | { defined = Some _; used = Some _} -> ()
    | { defined = None; used = None } -> raise (Impossible "one or the other")
  );

  (* less: could check unused static var decl? *)
  ()

(*****************************************************************************)
(* Unreachable code *)
(*****************************************************************************)
(* TODO *)

(*****************************************************************************)
(* Format checking *)
(*****************************************************************************)
(* TODO *)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let check_program ast =
  check_usedef ast
