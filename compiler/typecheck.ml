(* Copyright 2016 Yoann Padioleau, see copyright.txt *)
open Common

open Ast
module T = Type
module S = Storage
module E = Check

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* This module does many things:
 *  - it expands typedefs
 *  - it assigns a final (resolved) type to every identifiers
 *  - it assigns a final storage to every identifiers
 *  - it assigns at type to every expressions
 *  - it returns a typed AST and also transforms this AST
 *    (enum constants are replaced by their value, constant string replaces
 *     by a reference, etc)
 * 
 * Thanks to the naming done in parser.mly and the unambiguous Ast.fullname,
 * we do not have to handle scope here. 
 * Thanks to check.ml we do not have to check for inconcistencies or
 * redefinition of tags. We can assume everything is fine.
 * 
 * limitations compared to 5c:
 *  - no void* conversions 
 *    (clang does not either)
 *  - no struct equality by field equality. I use name equality.
 *    (who uses that anyway?)
 *  - no float enum
 *    (who uses that anyway?)
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* less: vlong? *)
type integer = int

(* Environment for typechecking *)
type env = {
  ids:  (Ast.fullname, idinfo) Hashtbl.t;
  structs: (Ast.fullname, Type.struct_kind * Type.structdef) Hashtbl.t;
  typedefs: (Ast.fullname, Type.t) Hashtbl.t;
  (* stricter: no float enum *)
  enums: (fullname, Type.integer_type) Hashtbl.t;
  (* stricter: no support for float enum constants either *)
  constants: (Ast.fullname, integer * Type.integer_type) Hashtbl.t;
}
  and idinfo = {
    typ: Type.t;
    sto: Storage.t;
    loc: Location_cpp.loc;
    (* typed initialisers (fake expression for function definitions) *)
    ini: Ast.initialiser option;
  }

(* less: could factorize things in error.ml? *)
type error = Check.error

let string_of_error err =
  Check.string_of_error err

exception Error of error

let type_error _t loc =
  (* less: dump t? *)
  raise (Error (E.ErrorMisc ("incompatible type", loc)))

let type_error2 _t1 _t2 loc =
  (* less: dump t1 and t2? *)
  raise (Error (E.ErrorMisc ("incompatible types", loc)))

(*****************************************************************************)
(* Types helpers *)
(*****************************************************************************)

(* if you declare multiple times the same global, we need to make sure
 * the types are the same. ex: 'extern int foo; ... int foo = 1;'
 * This is where we detect inconsistencies like 'int foo; void foo();'.
 * 
 * Because we expand typedefs, testing the equality of two types is simple.
 *)
let same_types t1 t2 =
  t1 = t2
  (* stricter: void* can not match any pointer *)
  (* stricter: struct equality by name, not fields *)
   

(* if you declare multiple times the same global, we should merge types. *)
let merge_types t1 t2 =
  t1

(* when processing enumeration constants, we want to keep the biggest type *)
(*
let max_types t1 t2 = ...
*)

(* when you apply an operation between two expressions, this
 * expression can be valid even if the types of those two expressions
 * are not the same. However, they must be "compatible". 
 * The compatibility policy depends on the operation of the expression.
 * 
 * less: better error messages, for instance when want to add 2 pointers.
 *)
let check_compatible_binary op t1 t2 loc =
  match op with
  | Arith Plus ->
    (match t1, t2 with
    | (T.I _ | T.F _), (T.I _ | T.F _)
    | T.Pointer _, T.I _
    | T.I _, T.Pointer _
      -> ()
    (* you can not add 2 pointers *)
    | _ -> type_error2 t1 t2 loc
    )
  | Arith Minus ->
    (match t1, t2 with
    | (T.I _ | T.F _), (T.I _ | T.F _)
    (* you can not sub a pointer to an int (but can sub an int to a pointer) *)
    | T.Pointer _, T.I _
      -> ()
    (* you can sub 2 pointers *)
    | T.Pointer t1, T.Pointer t2 when same_types t1 t2 -> ()
    | _ -> type_error2 t1 t2 loc
    )
  | Arith (Mul | Div) ->
    (match t1, t2 with
    | (T.I _ | T.F _), (T.I _ | T.F _) -> ()
    | _ -> type_error2 t1 t2 loc
    )
  | Arith (Mod   | And | Or | Xor  | ShiftLeft | ShiftRight ) ->
    (match t1, t2 with
    | (T.I _), (T.I _ ) 
      -> ()
    (* stricter: I do not allow T.Int _ with T.F _, 5c does (clang does not) *)
    | _ -> type_error2 t1 t2 loc
    )

  | Logical (Eq | NotEq  | Inf | Sup | InfEq | SupEq) ->
    (match t1, t2 with
    | (T.I _ | T.F _), (T.I _ | T.F _) -> ()
    | T.Pointer t1, T.Pointer t2 when same_types t1 t2 -> ()
    (* you can not compare two structures! no deep equality (nor arrays) *)
    | _ -> type_error2 t1 t2 loc
    )
  | Logical (AndLog | OrLog) ->
    (* stricter? should impose bool! *)

    (match t1 with
    | (T.I _ | T.F _ | T.Pointer _) ->
      (match t2 with
      | (T.I _ | T.F _ | T.Pointer _) -> ()
      | _ -> type_error t2 loc
      )
    | _ -> type_error t1 loc
    )
            

let check_compatible_assign op t1 t2 loc =
  match op with
  | SimpleAssign ->
    (match t1, t2 with
    | (T.I _ | T.F _), (T.I _ | T.F _) -> ()
    | T.Pointer t1, T.Pointer t2 when same_types t1 t2 -> ()
    | T.StructName (su1, name1), T.StructName (su2, name2) 
      when su1 = su2 && name1 = name2 -> ()
    | _ -> type_error2 t1 t2 loc
    )
  (* not exactly the same rule *)
  | OpAssign op ->
    (match op with
    | (Plus | Minus) ->
      (match t1, t2 with
      | (T.I _ | T.F _), (T.I _ | T.F _) -> ()
      (* you can not x += y or x-=y when both x and y are pointers
       * (even though you can do x - y because the result type is a long,
       * and so you can not assign than back into x)
       *)
      | T.Pointer _, T.I _ -> ()
      | _ -> type_error2 t1 t2 loc
      )
    | (Mul | Div) 
    | (Mod   | And | Or | Xor  | ShiftLeft | ShiftRight ) 
      -> check_compatible_binary (Arith op) t1 t2 loc
    )



(*****************************************************************************)
(* Storage helpers *)
(*****************************************************************************)

(* If you declare multiple times the same global, we need to make sure
 * the storage declarations are compatible and we need to compute the
 * final (resolved) storage.
 * This function works for toplevel entities (globals but also functions).
 *)
let merge_storage_toplevel name loc stoopt ini old =
  match stoopt, old.sto with
    (* The None cases first *)
  
    (* this is ok, a header file can declare many externs and a C file
     * can then selectively "implements" some of those declarations.
     *)
    | None, S.Extern -> S.Global
    | None, S.Global ->
        (* stricter: even clang does not say anything here *)
        if ini = None
        then raise (Error (E.Inconsistent (
          spf "useless redeclaration of '%s'" name, loc,
          "previous definition is here", old.loc)))
        else S.Global

    (* stricter: 5c just warns for this *)
    | (None | Some S.Extern), S.Static ->
      raise (Error (E.Inconsistent (
       spf "non-static declaration of '%s' follows static declaration" name,loc,
       "previous definition is here", old.loc)))
    | _, (S.Auto | S.Param) -> 
      raise (Impossible "globals can't be auto or param")

    (* The Some cases *)

    (* stricter: useless extern *)
    | Some S.Extern, (S.Global | S.Extern) ->
      raise (Error (E.Inconsistent (
        spf "useless extern declaration of '%s'" name, loc,
        "previous definition is here", old.loc)))

    | Some S.Auto, _ ->
      raise  (Error(E.ErrorMisc 
                ("illegal storage class for file-scoped entity", loc)))
    | Some S.Static, (S.Extern | S.Global) ->
      raise (Error (E.Inconsistent (
       spf "static declaration of '%s' follows non-static declaration" name,loc,
       "previous definition is here", old.loc)))

    | Some S.Static, S.Static ->
        if ini = None
        then raise (Error (E.Inconsistent (
          spf "useless redeclaration of '%s'" name, loc,
          "previous definition is here", old.loc)))
        else S.Static

    | Some (S.Global | S.Param), _ -> 
      raise (Impossible "param or global are not keywords")

(*****************************************************************************)
(* Constant expression evaluator *)
(*****************************************************************************)

exception NotAConstant

(* stricter: I do not handle float constants for enums *)
let rec eval env e0 =
  match e0.e with
  (* less: enough for big integers? *)
  | Int (s, inttype) -> int_of_string s
  | Id fullname ->
     if Hashtbl.mem env.constants fullname
     then
       let (i, inttype) = Hashtbl.find env.constants fullname in
       i
     else raise NotAConstant
  | Binary (e1, op, e2) ->
    let i1 = eval env e1 in
    let i2 = eval env e2 in
    (match op with
    | Arith op -> 
      (match op with
      | Plus -> i1 + i2
      | Minus -> i1 - i2
      | Mul -> i1 * i2
      | Div -> 
        (* stricter: error, not warning *)
        if i2 = 0 
        then raise (Error (E.ErrorMisc ("divide by zero", e0.e_loc)))
        else i1 / i2
      | Mod -> 
        if i2 = 0 
        then raise (Error (E.ErrorMisc ("modulo by zero", e0.e_loc)))
        else i1 mod i2
      | And -> i1 land i2
      | Or -> i1 lor i2
      | Xor -> i1 lxor i2
      | ShiftLeft -> i1 lsl i2
      (* less: could be asr! need type information! *)
      | ShiftRight -> i1 lsr i2
      )
    | Logical op ->
      (match op with
      | _ -> raise Todo
      )
    )
  | Unary (op, e) ->
    let i = eval env e in
    (match op with
    | UnPlus -> i
    | UnMinus -> - i
    | Tilde -> lnot i (* sure? *)
    | _ -> raise Todo
    )

  | _ -> raise Todo

(*****************************************************************************)
(* Other helpers *)
(*****************************************************************************)

(* we assume the typechecker has called expr() on 'e0' before, 
 * so Id of enum constants for example has been substituted to Int
 * and so are not considered an lvalue.
 *)
let rec lvalue e0 =
  match e0.e with
  | Id _ 
  | Unary (DeRef, _)
  | RecordAccess _
    -> true
  | Int _ | Float _ | String _
  | Binary _ | Unary _
    -> false
  | ArrayAccess _ | RecordPtAccess _ -> raise (Impossible "transformed before")
  | _ -> raise Todo

let array_to_pointer e =
  raise Todo

(*****************************************************************************)
(* AST Types to Types.t *)
(*****************************************************************************)

(* Expand typedefs and resolve constant expressions. *)
let rec type_ env typ0 =
  match typ0.t with
  | TBase t -> t
  | TPointer typ -> T.Pointer (type_ env typ)
  | TArray (eopt, typ) ->
      (match eopt with
      | None -> T.Array (None, type_ env typ)
      | Some e ->
        (try 
           let i = eval env e in
           T.Array (Some i, type_ env typ)
         with NotAConstant ->
           raise (Error (E.ErrorMisc ("array size must be a positive constant",
                                      typ0.t_loc)))
        )
      )
  | TFunction (tret, (tparams, tdots)) ->
      T.Func (type_ env tret, 
                tparams |> List.map (fun p -> type_ env p.p_type), tdots)
  | TStructName (su, fullname) -> T.StructName (su, fullname)
  (* expand enums *)
  | TEnumName fullname -> T.I (Hashtbl.find env.enums fullname)
  (* expand typedefs *)
  | TTypeName fullname -> Hashtbl.find env.typedefs fullname

(*****************************************************************************)
(* Expression typechecking *)
(*****************************************************************************)

let rec expr env e0 =
  match e0.e with
  | Int    (s, inttype)   -> { e0 with e_type = T.I inttype }
  | Float  (s, floattype) -> { e0 with e_type = T.F floattype }
  | String (s, t)         -> { e0 with e_type = t } |> array_to_pointer
  | Id fullname ->
     if Hashtbl.mem env.constants fullname
     then
       let (i, inttype) = Hashtbl.find env.constants fullname in
       { e0 with e = Int (spf "%d" i, inttype); e_type = T.I inttype }
     else
       let idinfo = Hashtbl.find env.ids fullname in
       { e0 with e_type = idinfo.typ } |> array_to_pointer
  | Sequence (e1, e2) -> 
    let e1 = expr env e1 in
    let e2 = expr env e2 in
    { e0 with e = Sequence (e1, e2); e_type = e2.e_type }

  | Binary (e1, op, e2) ->
    let e1 = expr env e1 in
    let e2 = expr env e2 in
    check_compatible_binary op e1.e_type e2.e_type e0.e_loc;
    (* TODO: add casts *)
    raise Todo

  | Unary (op, e) ->
    (match op with
    | UnPlus -> 
      let e = Binary ({e0 with e = Int ("0", T.Int T.Signed)}, Arith Plus, e) in
      expr env { e0 with e = e }
    | UnMinus -> 
      let e = Binary ({e0 with e = Int ("0", T.Int T.Signed)}, Arith Minus, e)in
      expr env { e0 with e = e }
    | Tilde ->
      let e = Binary ({e0 with e = Int ("-1", T.Int T.Signed)}, Arith Xor, e) in
      expr env { e0 with e = e }
    | Not ->
      let e = expr env e in
      (match e.e_type with
      (* less: what about T.Array ? *)
      | T.I _ | T.F _ | T.Pointer _ -> ()
      | _ -> type_error e.e_type e.e_loc 
      );
      { e0 with e = Unary (Not, e); e_type = T.int }

    | GetRef ->
      let e = expr env e in
      if not (lvalue (e))
      then raise (Error (E.ErrorMisc ("not an l-value", e0.e_loc)));
      (* less: warn if take address of array or function, ADDROP *)
      { e0 with e = Unary (GetRef, e); e_type = T.Pointer (e.e_type) }

    | DeRef ->
      let e = expr env e in
      (match e.e_type with
      (* less: what about T.Array ? *)
      | T.Pointer t -> 
        { e0 with e = Unary (DeRef, e); e_type = t } |> array_to_pointer
      | _ -> type_error e.e_type e.e_loc
      )
    )
  | Assign (op, e1, e2) ->
    let e1 = expr env e1 in
    let e2 = expr env e2 in
    if not (lvalue (e1))
    then raise (Error (E.ErrorMisc ("not an l-value", e0.e_loc)));
    check_compatible_assign op e1.e_type e2.e_type e0.e_loc;
    (* TODO: add casts *)
    raise Todo

  (* x[y] --> *(x+y) *)
  | ArrayAccess (e1, e2) ->
    let e = Unary (DeRef, { e0 with e = Binary (e1, Arith Plus, e2) }) in
    expr env { e0 with e = e }
  (* x->y --> ( *x).y *)
  | RecordPtAccess (e, name) ->
    let e = RecordAccess ({ e0 with e = Unary (DeRef, e)}, name) in
    expr env { e0 with e = e }

  | RecordAccess (e, name) ->
    let e = expr env e in
    (match e.e_type with
    | T.StructName (su, fullname) ->
      let (_su2, def) = Hashtbl.find env.structs fullname in
      (try
         let t = List.assoc name def in
         { e0 with e = RecordAccess (e, name); e_type = t } |> array_to_pointer
       with Not_found ->
         raise (Error (E.ErrorMisc (spf "not a member of struct/union: %s" name,
                                   e.e_loc)))
      )
    | _ -> type_error e.e_type e.e_loc
    )
  | Call (e, es) ->
    raise Todo
  | Cast (typ, e) ->
    let t = type_ env typ in
    let e = expr env e in
    (match e.e_type, t with
    | T.I _, (T.I _ | T.F _ | T.Pointer _ | T.Void)
    | T.F _, (T.I _ | T.F _ | T.Void)
    | T.Pointer _, (T.I _ | T.Pointer _ | T.Void)
      -> ()
    (* less: seems pretty useless *)
    | T.Void, T.Void -> ()
    (* less: seems pretty useless *)
    | T.StructName (su1, _) , T.StructName (su2, _) when su1 = su2 -> ()
    | T.StructName _, T.Void -> ()
    | _ -> type_error2 e.e_type t e0.e_loc
    );
    { e0 with e = Cast (typ, e); e_type = t } (* |> array_to_pointer ? *)

  | SizeOf(te) ->
    raise Todo
  | CondExpr (e1, e2, e3) ->
    raise Todo
  | Postfix(e, op) ->
    raise Todo
  | Prefix(op, e) ->
    raise Todo

  | ArrayInit _
  | RecordInit _
  | GccConstructor _
      -> raise Todo


and expropt env eopt = 
    match eopt with
    | None -> None
    | Some e -> Some (expr env e)


(*****************************************************************************)
(* Statement *)
(*****************************************************************************)

(* Boilerplate mostly.
 * expr() should not do any side effect on the environment, so we can
 * call recursively in any order stmt() and expr() (including the
 * reverse order of evaluation of OCaml for arguments).
 *)
let rec stmt env st0 =
  { st0 with s = 
    (match st0.s with
    | ExprSt e -> ExprSt (expr env e)
    | Block xs -> Block (List.map (stmt env) xs)
    | If (e, st1, st2) -> If (expr env e, stmt env st1, stmt env st2)
    (* todo: ensure e is an integer! not a pointer *)
    | Switch (e, xs) -> Switch (expr env e, stmt env xs)
    (* less: should enforce int expr? *)
    | Case (e, st) -> Case (expr env e, stmt env st)
    | Default st -> Default (stmt env st)

    | While (e, st) -> While (expr env e, stmt env st)
    | DoWhile (st, e) -> DoWhile (stmt env st, expr env e)
    | For (e1either, e2opt, e3opt, st) ->
      (* we may have to do side effect on the environment, so process that
       * first
       *)
      let e1either = 
        (match e1either with 
        | Left e1opt -> Left (expropt env e1opt)
        | Right decls -> raise Todo
        )
      in
      For (e1either, expropt env e2opt, expropt env e3opt, stmt env st)
    (* todo: check compatible with return type of function! so need
     * to know enclosing function type!
     *)
    | Return eopt -> Return (expropt env eopt)
    | Continue -> Continue
    | Break -> Break
    | Label (name, st) -> Label (name, stmt env st)
    | Goto name -> Goto name
    | Var vdecl -> raise Todo
    )
  }

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let check_and_annotate_program ast =

  let funcs = ref [] in

  let rec toplevel env = function
    | StructDef { su_kind=su; su_name=fullname; su_loc=loc; su_flds=flds }->
      Hashtbl.add env.structs fullname 
        (su, flds |> List.map 
            (fun {fld_name = name; fld_loc=_; fld_type = typ } -> 
              (name, type_ env typ)))

    | TypeDef { typedef_name = fullname; typedef_loc = loc; typedef_type =typ}->
      Hashtbl.add env.typedefs fullname (type_ env typ)

    | EnumDef { enum_name = fullname; enum_loc = loc; enum_constants = csts }
      ->
      (* stricter: no support for float enum constants *)
      let lastvalue = ref 0 in
      let maxt = ref (T.Int (T.Signed)) in
      csts |> List.iter (fun 
        { ecst_name = fullname; ecst_loc = loc; ecst_value = eopt } ->
          (match eopt with
          | Some e ->
            (try 
               (* less: should also return an integer type *)
               let i = eval env e in
               let t = (T.Int (T.Signed)) in
               (* todo: maxt := max_types !maxt t; *)
               Hashtbl.add env.constants fullname (i, t);
               lastvalue := i;
             with NotAConstant ->
               raise (Error (E.ErrorMisc (spf "enum not a constant: %s"
                                            (unwrap fullname), loc)))
            )
          | None ->
            (* todo: curt *)
            let t = (T.Int (T.Signed)) in
            Hashtbl.add env.constants fullname (!lastvalue, t);
          );
          incr lastvalue
      );
      Hashtbl.add env.enums fullname !maxt

    (* remember that VarDecl covers also prototypes *)
    | VarDecl { v_name = fullname; v_loc = loc; v_type = typ;
                v_storage = stoopt; v_init = eopt} ->
      let t = type_ env typ in
      let ini = expropt env eopt in

      (* step1: check for weird declarations *)
      (match t, ini, stoopt with
      | T.Func _, Some _, _ -> 
        raise (Error(E.ErrorMisc 
                     ("illegal initializer (only var can be initialized)",loc)))
      (* stricter: 5c says nothing, clang just warns *)
      | _, Some _, Some S.Extern ->
        raise (Error(E.ErrorMisc("'extern' variable has an initializer", loc)))
      | _ -> ()
      );

      (try 
         (* step2: check for weird redeclarations *)
         let old = Hashtbl.find env.ids fullname in

         (* check type compatibility *)
         if not (same_types t old.typ)
         then raise (Error (E.Inconsistent (
              (* less: could dump both type using vof_type *)
               spf "redefinition of '%s' with a different type" 
                 (unwrap fullname), loc,
               "previous definition is here", old.loc)))
         else
           let finalt = 
             merge_types t old.typ in
           let finalini = 
             match ini, old.ini with
             | Some x, None | None, Some x -> Some x
             | None, None -> None
             | Some x, Some y ->
               raise (Error (E.Inconsistent (
               spf "redefinition of '%s'" (unwrap fullname), loc,
               "previous definition is here", old.loc)))
           in
           (* check storage compatibility and compute final storage *)
           let finalsto = 
             merge_storage_toplevel (unwrap fullname) loc stoopt ini old in

           Hashtbl.replace env.ids fullname 
             {typ = finalt; sto = finalsto; loc = loc; ini = finalini }
       with Not_found ->
         let finalsto =
           match stoopt with
           | None -> S.Global
           | Some S.Extern -> S.Extern
           | Some S.Static -> S.Static
           | Some S.Auto -> 
             raise (Error(E.ErrorMisc 
                          ("illegal storage class for file-scoped entity",loc)))
           | Some (S.Global | S.Param) -> 
             raise (Impossible "global or param are not keywords")
         in
         Hashtbl.add env.ids fullname 
           {typ = t; sto = finalsto; loc = loc; ini = ini }
      )

    | FuncDef ({f_name=name; f_loc=loc; f_type=ftyp; 
               f_storage=stoopt; f_body=st;} as def) ->
      (* less: lots of code in common with Var_decl; we could factorize
       * but a few things are different still.
       *)
      let t = type_ env ({t = TFunction ftyp; t_loc = loc}) in
      let fullname = (name, 0) in
      (* we use a fake initializer for function definitions to 
       * be able to store those definitions in env.ids. That way
       * we can detect function redefinitions, useless redeclarations, etc.
       *)
      let ini = Some { e = Id fullname; e_loc = loc; e_type = Type.Void } in

      (try 
         (* check for weird redeclarations *)
         let old = Hashtbl.find env.ids fullname in

         (* check type compatibility *)
         if not (same_types t old.typ)
         then raise (Error (E.Inconsistent (
              (* less: could dump both type using vof_type *)
               spf "redefinition of '%s' with a different type" 
                 (unwrap fullname), loc,
               "previous definition is here", old.loc)))
         else
           let finalt = 
             merge_types t old.typ in
           let finalini = 
             match ini, old.ini with
             | Some x, None | None, Some x -> Some x
             | None, None -> None
             | Some x, Some y ->
               raise (Error (E.Inconsistent (
               spf "redefinition of '%s'" (unwrap fullname), loc,
               "previous definition is here", old.loc)))
           in
           (* check storage compatibility and compute final storage *)
           let finalsto = 
             merge_storage_toplevel (unwrap fullname) loc stoopt ini old in

           Hashtbl.replace env.ids fullname 
             {typ = finalt; sto = finalsto; loc = loc; ini = finalini }
       with Not_found ->
         let finalsto =
           match stoopt with
           | None -> S.Global
           | Some S.Static -> S.Static
           (* different than for VarDecl here *)
           | Some S.Extern -> 
             raise(Error(E.ErrorMisc("'extern' function with initializer",loc)))
           | Some S.Auto -> 
             raise (Error(E.ErrorMisc 
                          ("illegal storage class for file-scoped entity",loc)))
           | Some (S.Global | S.Param) -> 
             raise (Impossible "global or param are not keywords")
         in
         Hashtbl.add env.ids fullname 
           {typ = t; sto = finalsto; loc = loc; ini = ini }
      );

      (* add params in environment before process st *)
      let (tret, (tparams, _dots)) = ftyp in
      tparams |> List.iter (fun p ->
        p.p_name |> Common.if_some (fun fullname ->
          Hashtbl.add env.ids fullname 
            {typ = type_ env p.p_type; sto = S.Param; loc = loc; ini = None }
        )
      );
      (* the expressions inside the statements are now annontated with types *)
      let st = stmt env st in
      funcs := { def with f_body = st }::!funcs;
  in

  let env = {
    ids = Hashtbl.create 101;
    structs = Hashtbl.create 101;
    typedefs = Hashtbl.create 101;
    enums = Hashtbl.create 101;
    constants = Hashtbl.create 101;
  }
  in
  ast |> List.iter (toplevel env);

  env, List.rev !funcs
