(*s: Check.ml *)
(* Copyright 2016 Yoann Padioleau, see copyright.txt *)
open Common
open Fpath_.Operators
open Either

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

(*s: type [[Check.usedef]] *)
type usedef = {
  mutable defined: Ast.loc option;
  mutable used: Ast.loc option;
}
(*e: type [[Check.usedef]] *)

(*s: type [[Check.env]] *)
type env = {
  ids:      (fullname, usedef * Ast.idkind) Hashtbl.t;
  tags:     (fullname, usedef * Ast.tagkind) Hashtbl.t;

  (* to reset after each function (because labels have a function scope) *)
  mutable labels:   (string, usedef) Hashtbl.t;
  (* block scope *)
  mutable local_ids: fullname list;

  (* todo: inbreakable: bool; incontinueable: bool *)
}
(*e: type [[Check.env]] *)

(*s: type [[Check.error]] *)
type error = 
  | Inconsistent of 
      string * Location_cpp.loc * (* error here *) 
      string * Location_cpp.loc   (* previous decl/def/whatever here *)
  | Misc of string * Location_cpp.loc
(*e: type [[Check.error]] *)

(*s: function [[Check.string_of_error]] *)
let string_of_error err =
  match err with
  | Inconsistent (s1, loc1, s2, loc2) ->
    let (file1, line1) = Location_cpp.final_loc_of_loc loc1 in
    let (file2, line2) = Location_cpp.final_loc_of_loc loc2 in
    spf "%s:%d error: %s\n%s:%d note: %s" !!file1 line1 s1 !!file2 line2 s2
  | Misc (s, loc) ->
    let (file, line) = Location_cpp.final_loc_of_loc loc in
    spf "%s:%d error: %s" !!file line s
(*e: function [[Check.string_of_error]] *)

(*s: exception [[Check.Error]] *)
exception Error of error
(*e: exception [[Check.Error]] *)

(*s: constant [[Check.failhard]] *)
let failhard = ref false
(*e: constant [[Check.failhard]] *)

(*s: function [[Check.error]] *)
let error err =
  if !failhard
  then raise (Error err)
  else Logs.err (fun m -> m "%s" (string_of_error err))
(*e: function [[Check.error]] *)
 
(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(*s: function [[Check.inconsistent_tag]] *)
let inconsistent_tag fullname loc usedef =
  let locbefore = 
    match usedef with
    (* ocaml-light: | { defined = Some loc; _ } | { used = Some loc; _ } *)
    | { defined = Some loc; used = _ } -> loc
    | { used = Some loc; defined = _ } -> loc
    | _ -> raise (Impossible "must have a def or a use")
  in
  error (Inconsistent (
    spf "use of '%s' with tag type that does not match previous declaration "
      (unwrap fullname), loc,
    "previous use is here", locbefore
  ))
(*e: function [[Check.inconsistent_tag]] *)

(*s: function [[Check.check_inconsistent_or_redefined_tag]] *)
let check_inconsistent_or_redefined_tag env fullname tagkind loc =
  try 
    let (usedef, oldtagkind) = Hashtbl.find env.tags fullname in
    if tagkind <> oldtagkind
    then inconsistent_tag fullname loc usedef;
    (* the tag may not have be defined, as in a previous 'struct Foo x;' *)
    usedef.defined |> Option.iter (fun locdef ->
      error (Inconsistent (spf "redefinition of '%s'" (unwrap fullname), loc,
                     "previous definition is here", locdef))
    );
    (* now it's defined *)
    usedef.defined <- Some loc;
  with Not_found ->
    Hashtbl.add env.tags fullname ({defined = Some loc; used = None;}, tagkind)
(*e: function [[Check.check_inconsistent_or_redefined_tag]] *)


(*s: function [[Check.inconsistent_id]] *)
let inconsistent_id fullname loc usedef =
  let locbefore = 
    match usedef with
    | { defined = Some loc; used = _ } -> loc
    | { defined = None; used = _ } -> raise (Impossible "id always defined first")
  in
  error (Inconsistent (
    spf "redefinition of '%s' " (unwrap fullname), loc,
    "previous definition is here", locbefore
  ))
(*e: function [[Check.inconsistent_id]] *)


(*s: function [[Check.check_inconsistent_or_redefined_id]] *)
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
(*e: function [[Check.check_inconsistent_or_redefined_id]] *)

(*s: function [[Check.check_unused_locals]] *)
let check_unused_locals env =
  (* less: could also delete entries in env.ids *)
  env.local_ids |> List.iter (fun fullname ->
    let (usedef, idkind) = Hashtbl.find env.ids fullname in
    assert (idkind = IdIdent);
    match usedef with
    | { defined = Some loc; used = None } ->
        (* 5c says whether 'auto' or 'param' *) 
        Error.warn 
          (spf "variable declared and not used: '%s'" (unwrap fullname)) loc
    | { defined = None; used = _} -> 
          raise (Impossible "locals are always defined")
    | { defined = _; used = Some _ } -> ()
(*e: function [[Check.check_unused_locals]] *)
  )


(*****************************************************************************)
(* Use/Def *)
(*****************************************************************************)

(*s: function [[Check.check_usedef]] *)
(* use of undefined, redefined, redeclared, unused, inconsistent tags, etc. *)
let check_usedef program =

  let rec toplevel env = function
    | StructDef { su_kind=su; su_name=fullname; su_loc=loc; su_flds=flds }->
      (* checking the tag *)

      let tagkind = Ast.tagkind_of_su su in
      check_inconsistent_or_redefined_tag env fullname tagkind loc;

      (* checking the fields *)

      let hflds = Hashtbl.create 11 in
      flds |> List.iter 
       (fun {fld_name = name; fld_loc = loc; fld_type = typ} ->
          (* stricter: 5c reports at use time, clang does immediately *)
          if Hashtbl.mem hflds name
          then error (Inconsistent (spf "duplicate member '%s'" name, loc,
                                    "previous declaration is here", 
                                    Hashtbl.find hflds name));
          Hashtbl.add hflds name loc;
          type_ env typ
      )

    | EnumDef { enum_name = fullname; enum_loc = loc; enum_constants = csts }->
      (* checking the tag *)

      let tagkind = TagEnum in
      check_inconsistent_or_redefined_tag env fullname tagkind loc;

      (* checking the constants *)

      csts |> List.iter 
          (fun { ecst_name = fullname; ecst_loc = loc; ecst_value = eopt } ->
            check_inconsistent_or_redefined_id env fullname IdEnumConstant loc;
            eopt |> Option.iter (expr env)
          );

    | TypeDef { typedef_name = fullname; typedef_loc = loc; typedef_type =typ}->
      check_inconsistent_or_redefined_id env fullname IdTypedef loc;
      type_ env typ

    (* todo: if use struct tags params, they must be complete at this point *)
    | FuncDef { f_name = name; f_loc = loc; f_type = ftyp; f_body = st; f_storage = _ } ->
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
      tparams |> List.iter (fun { p_name = fullnameopt; p_loc; p_type = _} ->
        fullnameopt |> Option.iter (fun fullname ->
          check_inconsistent_or_redefined_id env fullname IdIdent p_loc;
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
            error (Misc (spf "use of undeclared label '%s'" name, loc))
        | { defined = Some loc; used = None } ->
            Error.warn (spf "label declared and not used '%s'" name) loc
        | { defined = None; used = None } -> 
          raise (Impossible "at least one of used or defined")
      );
        
    | VarDecl { v_name = fullname; v_loc; v_type = t; v_init = eopt; v_storage = _} ->
      (if Hashtbl.mem env.ids fullname &&
          snd (Hashtbl.find env.ids fullname) = IdIdent
      (* this can be ok, you can redeclare toplevel identifiers as you
       * can give a final storage. It depends on the situation, 
       * see typecheck.ml
       *)
       then ()
       else check_inconsistent_or_redefined_id env fullname IdIdent v_loc
      );
      type_ env t;
      eopt |> Option.iter (expr env)

  and stmt env st0 =
    match st0.s with
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
    (* ocaml-light: | While (e, st)  | Switch (e, st) | Case (e, st) *)
    | While (e, st) ->
        expr env e;
        stmt env st;
    | Switch (e, st) ->
        expr env e;
        stmt env st;
    | Case (e, st) ->
        expr env e;
        stmt env st;
    | DoWhile (st, e) ->
        stmt env st;
        expr env e
    | For (e1either, e2opt, e3opt, st) ->
        (* new block scope again *)
        let env = { env with local_ids = [] } in
        (match e1either with
        | Left e1opt -> e1opt |> Option.iter (expr env)
        | Right decls ->
            decls |> List.iter (fun decl -> 
              stmt env ({s = Var decl; s_loc = decl.v_loc }) 
            )
        );
        e2opt |> Option.iter (expr env);
        e3opt |> Option.iter (expr env);
        stmt env st;
        (* check block scope *)
        check_unused_locals env

    | Return eopt -> eopt |> Option.iter (expr env)
    (* todo: check that inside something that be continue/break *)
    | Continue | Break -> ()

    (* checks on labels are done in FuncDef once we analyzed the whole body *)
    | Label (name, st) ->
        (try
           let usedef = Hashtbl.find env.labels name in
           usedef.defined |> Option.iter (fun locprev ->
             error (Inconsistent (spf "redefinition of label '%s'" name, 
                                  st0.s_loc,
                                  "previous definition is here", locprev))
           );
           usedef.defined <- Some st0.s_loc;
         with Not_found ->
           Hashtbl.add env.labels name 
             {defined = Some st0.s_loc; used = None}
        );
        stmt env st;
    | Goto name ->
        (try 
           let usedef = Hashtbl.find env.labels name in
           usedef.used <- Some st0.s_loc
         with Not_found ->
           Hashtbl.add env.labels name { defined = None; used = Some st0.s_loc }
        )
    | Default st -> stmt env st

    | Var { v_name = fullname; v_loc = loc; v_type = typ; v_init = eopt; v_storage = _ } ->
      (* less: before adding in environment? can have recursive use? *)
      eopt |> Option.iter (expr env);
    (* todo: if local VarDEcl, can actually have stuff nested like
     *  extern int i;  in which case we must go back to global
     *  scope for i! so rewrite AST? or just in typecheck.ml
     *  generate right storage for it.
     * can also be nested prototype (but I should forbid it
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
           (* todo: can be USED or SET *)
           raise (Impossible (spf "ids are always declared first: %s" 
                                (unwrap fullname)))
        )
    | Call (e, es) -> exprs env (e::es)
    (* ocaml-light: | Assign (_, e1, e2) | Binary (e1, _, e2) | Sequence (e1, e2) | ArrayAccess (e1, e2) *)
    | Assign (_, e1, e2) -> exprs env [e1; e2]
    | Binary (e1, _, e2) -> exprs env [e1; e2]
    | Sequence (e1, e2) -> exprs env [e1; e2]
    | ArrayAccess (e1, e2) -> exprs env [e1; e2]
    (* ocaml-light: | RecordAccess (e, _) | RecordPtAccess (e, _) 
    | Postfix (e, _) | Prefix (_, e) | Unary (_, e) *)
    | RecordAccess (e, _) -> expr env e
    | RecordPtAccess (e, _) -> expr env e
    | Postfix (e, _) -> expr env e
    | Prefix (_, e) -> expr env e
    | Unary (_, e) -> expr env e
    (* ocaml-light: | Cast (typ, e) | GccConstructor (typ, e) *)
    | Cast (typ, e) ->
      type_ env typ;
      expr env e
    | GccConstructor (typ, e) ->
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
        eopt |> Option.iter (expr env);
        expr env e
      )
    | RecordInit xs -> xs |> List.iter (fun (_, e) -> expr env e)

  and exprs env xs = xs |> List.iter (expr env)

  and type_ env = fun typ ->
    match typ.t with
    | TBase _ -> ()
    | TPointer t -> type_ env t
    | TArray (eopt, t) -> 
      eopt |> Option.iter (expr env);
      type_ env t;
    | TFunction (tret, (params, _dots)) ->
      type_ env tret;
      params |> List.iter (fun p ->
        (* nothing to do with p.p_name here *)
        type_ env p.p_type
      )
    | TStructName (_, _) | TEnumName _ ->
      let tagkind, fullname = 
        match typ.t with
        | TStructName (su, fullname) -> Ast.tagkind_of_su su, fullname
        | TEnumName fullname -> TagEnum, fullname
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
  env.tags |> Hashtbl.iter (fun fullname (usedef, _idkind) ->
    match usedef with
    | { used = Some loc; defined = None } ->
      error (Misc (spf "use of tag '%s' that is never completed"
                          (unwrap fullname), loc))
    (* this can happen, header file are big and can cover multiple modules *)
    | { defined = Some _; used = None } -> ()
    (* perfect *)
    | { defined = Some _; used = Some _} -> ()
    | { defined = None; used = None } -> raise (Impossible "one or the other")
  );

  (* less: could check unused static var decl? *)
  ()
(*e: function [[Check.check_usedef]] *)

(*****************************************************************************)
(* Unreachable code *)
(*****************************************************************************)
(* TODO *)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

(*s: function [[Check.check_program]] *)
let check_program (ast, _locs) =
  check_usedef ast
(*e: function [[Check.check_program]] *)
(*e: Check.ml *)
