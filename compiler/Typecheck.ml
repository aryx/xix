(*s: Typecheck.ml *)
(* Copyright 2016, 2017 Yoann Padioleau, see copyright.txt *)
open Common
open Eq.Operators
open Either

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
 *  - it assigns a type to every expressions
 *  - it returns a typed AST and also transforms this AST
 *      * enum constants are replaced by their value
 *      * unary +/-/~ are replaced with binary op
 *      * add & before arrays in certain context
 *      * add explicit Cast operations
 *      * convert ArrayAccess in pointer arithtmetic operation
 * 
 * Thanks to the naming done in parser.mly and the unambiguous Ast.fullname,
 * we do not have to handle scope here. 
 * Thanks to check.ml we do not have to check for inconsistencies or
 * redefinition of tags. We can assume everything is fine.
 * 
 * limitations compared to 5c:
 *  - no 'void*' conversions 
 *    (clang does not either? but we do accept so void* vs xxx* )
 *  - no struct equality by field equality. I use name equality.
 *    (who uses that anyway?)
 *  - no float enum
 *    (who uses that anyway?)
 *  - no 0 to nil automatic cast inserted
 *    (I prefer stricter typechecking)
 * 
 * todo: 
 *  - const checking (in check_const.ml?) need types!
 *  - format checking (in check_format.ml?) need types!
 *  - misc checks
 *    * stupid shift bits (need constant evaluation) outside width
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* less: vlong? *)
(*s: type [[Typecheck.integer]] *)
type integer = int
(*e: type [[Typecheck.integer]] *)

(*s: type [[Typecheck.idinfo]] *)
type idinfo = {
    typ: Type.t;
    sto: Storage.t;
    loc: Location_cpp.loc;
    (* typed initialisers (fake expression for function definitions) *)
    ini: Ast.initialiser option;
  }
(*e: type [[Typecheck.idinfo]] *)
[@@deriving show]

(* for ocaml-light to work without deriving *)
let show_typed_program _ = "NO DERIVING"
[@@warning "-32"]

(* alt: Frontend.result, Frontend.entities, Frontend.t, Typecheck.result *)
(*s: type [[Typecheck.typed_program]] *)
type typed_program = {
  (* resolved type and storage information for identifiers and tags *)
  ids: (Ast.fullname, idinfo) Hashtbl_.t;

  (* resolved struct definitions *)
  structs: (Ast.fullname, Type.struct_kind * Type.structdef) Hashtbl_.t;

  (* functions annotated with types for each expression nodes
   * (so you can more easily generate code later).
   * 
   * The enum constants should also be internally resolved and replaced
   * with constants and some constant expressions (e.g., for
   * array size) should also be resolved (and evaluated).
   *)
  funcs: Ast.func_def list;
}
(*e: type [[Typecheck.typed_program]] *)
[@@deriving show]

(*s: type [[Typecheck.env]] *)
(* Environment for typechecking *)
type env = {

  (* those 2 fields will be returned ultimately by check_and_annotate_program *)
  ids_:  (Ast.fullname, idinfo) Hashtbl.t;
  structs_: (Ast.fullname, Type.struct_kind * Type.structdef) Hashtbl.t;

  (* internal *)
  typedefs: (Ast.fullname, Type.t) Hashtbl.t;
  (* stricter: no float enum *)
  enums: (fullname, Type.integer_type) Hashtbl.t;

  (* stricter: no support for float enum constants either *)
  constants: (Ast.fullname, integer * Type.integer_type) Hashtbl.t;

  (* return type of function; used to typecheck Return *)
  return_type: Type.t;

  (*s: [[Typecheck.env]] other fields *)
  (* used to add some implicit GetRef for arrays and functions *)
  expr_context: expr_context;
  (*e: [[Typecheck.env]] other fields *)
}
(*e: type [[Typecheck.env]] *)
(*s: type [[Typecheck.expr_context]] *)
and expr_context = 
  | CtxWantValue 
  (*s: [[Typecheck.expr_context]] other cases *)
  | CtxGetRef 
  (*x: [[Typecheck.expr_context]] other cases *)
  | CtxSizeof
  (*e: [[Typecheck.expr_context]] other cases *)
(*e: type [[Typecheck.expr_context]] *)

(* less: could factorize things in error.ml? *)
(*s: type [[Typecheck.error]] *)
type error = Check.error
(*e: type [[Typecheck.error]] *)

(*s: function [[Typecheck.string_of_error]] *)
let string_of_error err =
  Check.string_of_error err
(*e: function [[Typecheck.string_of_error]] *)

(*s: exception [[Typecheck.Error]] *)
exception Error of error
(*e: exception [[Typecheck.Error]] *)

(*s: function [[Typecheck.type_error]] *)
let type_error _t loc =
  (* less: dump t? *)
  raise (Error (E.Misc ("incompatible type", loc)))
(*e: function [[Typecheck.type_error]] *)

(*s: function [[Typecheck.type_error2]] *)
(* todo: for op xxx *)
let type_error2 t1 t2 loc =
  let s1 = Dumper_.s_of_any (FinalType t1) in
  let s2 = Dumper_.s_of_any (FinalType t2) in
  raise (Error (E.Misc (spf "incompatible types (%s and %s)" s1 s2, loc)))
(*e: function [[Typecheck.type_error2]] *)

(*****************************************************************************)
(* Types helpers *)
(*****************************************************************************)

(*s: function [[Typecheck.same_types]] *)
(* if you declare multiple times the same global, we need to make sure
 * the types are the same. ex: 'extern int foo; ... int foo = 1;'
 * This is where we detect inconsistencies like 'int foo; void foo();'.
 * 
 * Because we expand typedefs before calling same_types, and because
 * we do struct equality by name not fields, testing the equality of 
 * two types is simple.
 *)
let same_types (t1 : Type.t) (t2 : Type.t) : bool =
  match t1, t2 with

  (* 'void*' can match any pointer! The generic trick of C
   * (but only when the pointer is at the top of the type).
   *)
  | T.Pointer T.Void, T.Pointer _      -> true
  | T.Pointer _,      T.Pointer T.Void -> true

  (* stricter: struct equality by name, not by fields *)
  | _ -> t1 =*= t2
(*e: function [[Typecheck.same_types]] *)
   
(*s: function [[Typecheck.merge_types]] *)
(* if you declare multiple times the same global, we must merge types. *)
let merge_types t1 _t2 =
  t1
  (* TODO? what is doing 5c? *)
(*e: function [[Typecheck.merge_types]] *)

(* when processing enumeration constants, we want to keep the biggest type *)
(*
let max_types t1 t2 = ...
*)

(*s: function [[Typecheck.check_compatible_binary]] *)
(* when you apply an operation between two expressions, this
 * expression can be valid even if the types of those two expressions
 * are not the same. However, they must be "compatible". 
 * The compatibility policy depends on the operation of the expression.
 * 
 * less: better error messages, for instance when want to add 2 pointers.
 *)
let check_compatible_binary op (t1 : Type.t) (t2 : Type.t) loc : unit =
  match op with
  | Arith Plus ->
    (match t1, t2 with
    | (T.I _ | T.F _), (T.I _ | T.F _)
    | T.Pointer _, T.I _
    | T.I _, T.Pointer _
      -> ()
    (* you can not add 2 pointers *)
    | T.Pointer _, T.Pointer _
    | _ -> type_error2 t1 t2 loc
    )
  | Arith Minus ->
    (match t1, t2 with
    | (T.I _ | T.F _), (T.I _ | T.F _)
    (* you can not sub a pointer to an int (but can sub an int to a pointer) *)
    | T.Pointer _, T.I _
      -> ()
    (* you can sub 2 pointers (if they have the same types, and the
     * result is a long). same_types() will allow a void* to match any pointer.
     *)
    | T.Pointer _, T.Pointer _ when same_types t1 t2 -> ()
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
    | T.Pointer _, T.Pointer _ when same_types t1 t2 -> ()
    (* you can not compare two structures! no deep equality (nor arrays) *)
    | _ -> type_error2 t1 t2 loc
    )
  | Logical (AndLog | OrLog) ->
    (* stricter? should impose Bool! *)
    (match t1 with
    | (T.I _ | T.F _ | T.Pointer _) ->
      (match t2 with
      | (T.I _ | T.F _ | T.Pointer _) -> ()
      | _ -> type_error t2 loc
      )
    | _ -> type_error t1 loc
    )
(*e: function [[Typecheck.check_compatible_binary]] *)

(*s: function [[Typecheck.result_type_binary]] *)
let result_type_binary (t1 : Type.t) (t2 : Type.t) : Type.t =
  match t1, t2 with
  | T.I (T.Char, T.Signed), (T.I _ | T.F _ | T.Pointer _) -> t2

  | T.I (T.Char, T.Unsigned), T.I (x, _) -> T.I (x, T.Unsigned)
  | T.I (T.Char, T.Unsigned), (T.F _ | T.Pointer _) -> t2

  | T.I (T.Short, T.Signed), (T.I ((T.Char|T.Short), sign)) -> 
      T.I (T.Short, sign)
  | T.I (T.Short, T.Signed), (T.I _ | T.F _ | T.Pointer _) -> t2

  | T.I (T.Short, T.Unsigned), (T.I ((T.Char|T.Short), _)) -> 
      T.I (T.Short, T.Unsigned)
  | T.I (T.Short, T.Unsigned), T.I (x, _) -> T.I (x, T.Unsigned)
  | T.I (T.Short, T.Unsigned), (T.F _ | T.Pointer _) -> t2

  | T.I (T.Int, T.Signed), (T.I ((T.Char|T.Short|T.Int), sign)) -> 
      T.I (T.Int, sign)
  | T.I (T.Int, T.Signed), (T.I _ | T.F _ | T.Pointer _) -> t2

  | T.I (T.Int, T.Unsigned), (T.I ((T.Char|T.Short|T.Int), _)) -> 
      T.I (T.Int, T.Unsigned)
  | T.I (T.Int, T.Unsigned), T.I (x, _) -> T.I (x, T.Unsigned)
  | T.I (T.Int, T.Unsigned), (T.F _ | T.Pointer _) -> t2

  | T.I (T.Long, T.Signed), (T.I ((T.Char|T.Short|T.Int|T.Long), sign))-> 
      T.I (T.Long, sign)
  | T.I (T.Long, T.Signed), (T.I _ | T.F _ | T.Pointer _) -> t2

  | T.I (T.Long, T.Unsigned), (T.I ((T.Char|T.Short|T.Int|T.Long), _)) -> 
      T.I (T.Long, T.Unsigned)
  | T.I (T.Long, T.Unsigned), T.I (x, _) -> T.I (x, T.Unsigned)
  | T.I (T.Long, T.Unsigned), (T.F _ | T.Pointer _) -> t2

  | T.I (T.VLong, T.Signed),(T.I ((T.Char|T.Short|T.Int|T.Long|T.VLong),sign))->
      T.I (T.VLong, sign)
  | T.I (T.VLong, T.Signed), (T.F _ | T.Pointer _) -> t2

  | T.F T.Float, (T.I _ | T.F T.Float) -> T.F T.Float
  | T.F T.Float, T.F T.Double -> T.F T.Double
  | T.F T.Float, T.Pointer _ -> t2

  | T.F T.Double, (T.I _ | T.F _) -> T.F T.Double
  | T.F T.Double, T.Pointer _ -> t2

  | T.Pointer _, (T.I _ | T.F _) -> t1

  (* see same_types special handling of void* pointers *)
  | T.Pointer T.Void, T.Pointer _ -> t2
  | T.Pointer _, T.Pointer T.Void -> t1

  | T.Pointer _, T.Pointer _ ->
    assert (t1 =*= t2);
    t1

  | _ -> raise (Impossible "case should be forbidden by compatibility policy")
(*e: function [[Typecheck.result_type_binary]] *)

(*s: function [[Typecheck.check_compatible_assign]] *)
(* less: could run typ_ext hooks here? and return a new node? for
 * unnamed_inheritance.c?
 *)
let check_compatible_assign op (t1 : Type.t) (t2 : Type.t) loc : unit =
  match op with
  | Eq_ ->
    (match t1, t2 with
    | (T.I _ | T.F _), (T.I _ | T.F _) -> ()
    (* 'void*' special handling done in same_types() *)
    | T.Pointer _, T.Pointer _ when same_types t1 t2 -> ()
    | T.StructName (su1, name1), T.StructName (su2, name2) 
      when su1 =*= su2 && name1 =*= name2 -> ()
    | _ -> type_error2 t1 t2 loc
    )
  (* not exactly the same rule than in check_compatible_binary *)
  | OpAssign op ->
    (match op with
    | (Plus | Minus) ->
      (match t1, t2 with
      | (T.I _ | T.F _), (T.I _ | T.F _) -> ()
      (* you can not x += y or x-=y when both x and y are pointers
       * even though you can do x - y because the result type is a long
       * (and so you can not assign than back into x).
       *)
      | T.Pointer _, T.I _ -> ()
      | _ -> type_error2 t1 t2 loc
      )
    | (Mul | Div) 
    | (Mod   | And | Or | Xor  | ShiftLeft | ShiftRight ) 
      -> check_compatible_binary (Arith op) t1 t2 loc
    )
(*e: function [[Typecheck.check_compatible_assign]] *)

(*s: function [[Typecheck.check_args_vs_params]] *)
(* 5c: was called tcoma() *)
let rec check_args_vs_params (es : expr list) tparams (varargs : bool) loc =
  match es, tparams, varargs with

  (* stricter? confusing to have foo() and foo(void) *)
  | [], ([] | [T.Void]), _ -> ()

  | [], _, _ -> 
    raise (Error (E.Misc ("not enough function arguments", loc)))

  | _e::_es, [], false -> 
    raise (Error (E.Misc ("too many function arguments", loc)))

  | e::es, [], true -> 
    (match e.e_type with
    (* ??? *)
    | T.I _ | T.F _ | T.Pointer _ | T.StructName _ -> ()
    (* TODO: enumerate possible remaining, and why type_error? *)
    | _ -> type_error e.e_type loc
    );
    check_args_vs_params es [] true loc

  | e::es, t::ts, _ ->
    (match e.e_type with
    | T.I _ | T.F _ | T.Pointer _ | T.StructName _ -> ()
    (* TODO: enumerate possible remaining, and why type_error? *)
    | _ -> type_error e.e_type loc
    );
    (try 
       (* todo: convert to int small types? see tcoma *)
       check_compatible_assign Eq_ t e.e_type e.e_loc
     with Error _ ->
       (* TODO? actual error message of 5c? *)
       raise (Error (E.Misc ("argument prototype mismatch", e.e_loc)))
    );
    check_args_vs_params es ts varargs loc
(*e: function [[Typecheck.check_args_vs_params]] *)
    
(*****************************************************************************)
(* Storage helpers *)
(*****************************************************************************)
(*s: function [[Typecheck.merge_storage_toplevel]] *)
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
        if ini =*= None
        then raise (Error (E.Inconsistent (
          spf "useless redeclaration of '%s'" name, loc,
          "previous definition is here", old.loc)))
        else S.Global

    (* stricter: 5c just warns for this *)
    | (None | Some S.Extern), S.Static ->
      raise (Error (E.Inconsistent (
       spf "non-static declaration of '%s' follows static declaration" name,loc,
       "previous definition is here", old.loc)))
    | _, (S.Local | S.Param) -> 
      raise (Impossible "globals can't be auto or param")

    (* The Some cases *)

    (* stricter: useless extern *)
    | Some S.Extern, (S.Global | S.Extern) ->
      raise (Error (E.Inconsistent (
        spf "useless extern declaration of '%s'" name, loc,
        "previous definition is here", old.loc)))

    | Some S.Local, _ ->
      raise (Error(E.Misc("illegal storage class for file-scoped entity", loc)))
    | Some S.Static, (S.Extern | S.Global) ->
      raise (Error (E.Inconsistent (
       spf "static declaration of '%s' follows non-static declaration" name,loc,
       "previous definition is here", old.loc)))

    | Some S.Static, S.Static ->
        if ini =*= None
        then raise (Error (E.Inconsistent (
          spf "useless redeclaration of '%s'" name, loc,
          "previous definition is here", old.loc)))
        else S.Static

    | Some (S.Global | S.Param), _ -> 
      raise (Impossible "param or global are not keywords")
(*e: function [[Typecheck.merge_storage_toplevel]] *)

(*****************************************************************************)
(* Other helpers *)
(*****************************************************************************)
(*s: function [[Typecheck.lvalue]] *)
(* we assume the typechecker has called expr() on 'e0' before, 
 * so Id of enum constants for example has been substituted to Int
 * and so are not considered an lvalue.
 * 5c: was cached in an 'addable' field
 *)
let lvalue (e0 : expr) : bool =
  match e0.e with
  | Id _ 
  | Unary (DeRef, _)
  (* todo: lvalue only if leftpart is a lvalue. But when it can not be
   * a lvalue? if bitfield?
   *)
  | RecordAccess _
    -> true

  (* Strings are transformed at some point in Id.
   * We must consider them as an lvalue, because an Id is an lvalue 
   * and because if a string is passed as an argument to a function, we want
   * to pass the address of this string (see array_to_pointer()).
   *)
  | String _ -> 
    (* raise (Impossible "transformed before") *)
    true

  | Int _ | Float _
  | Binary _ 
  | Unary ((GetRef | UnPlus |  UnMinus | Tilde | Not), _)
    -> false
  | ArrayAccess _ | RecordPtAccess _ -> raise (Impossible "transformed before")
  (* TODO? what remains? should be just false no? *)
  | _ -> raise Todo
(*e: function [[Typecheck.lvalue]] *)

(*s: function [[Typecheck.array_to_pointer]] *)
(* When you mention an array in a context where you want to access the array
 * content, we prefix the array with a '&' and change its type from a T.Array
 * to a T.Pointer. This allows in turn to write typechecking rules
 * mentioning only Pointer (see for example check_compatible_binary).
 *)
let array_to_pointer (env : env) (e : expr) : expr =
  match e.e_type with
  | T.Array (_, t) ->

    (match env.expr_context with
    | CtxWantValue -> 
      if not (lvalue (e))
      then raise (Error (E.Misc ("not an l-value", e.e_loc)));

      { e = Unary (GetRef, e); e_type = T.Pointer t; e_loc = e.e_loc }
    (*s: [[array_to_pointer()]] when [[Array]] case, match context cases *)
    | CtxGetRef -> 
      Error.warn "address of array ignored" e.e_loc;
      e
    (*x: [[array_to_pointer()]] when [[Array]] case, match context cases *)
    | CtxSizeof -> e
    (*e: [[array_to_pointer()]] when [[Array]] case, match context cases *)
    )

  (* stricter? do something for Function? or force to put address? *)
  | T.Func _ ->
    (match env.expr_context with
    | CtxWantValue -> 
      Error.warn "you should get the address of the function" e.e_loc;
      e
    | _ -> e
    )
  
  | _ -> e
(*e: function [[Typecheck.array_to_pointer]] *)

(*s: function [[Typecheck.unsugar_anon_structure_element]] *)
(* X.foo --> X.|sym42|.foo *)
let rec unsugar_anon_structure_element (env : env) e0 e name def =
  
  let res = ref [] in

  def |> List.iter (fun (fldname, t) ->
    if fldname = name
    then res |> Stack_.push { e0 with e = RecordAccess (e, name); e_type = t }
    else
      if Ast.is_gensymed fldname
      then
        (match t with
        | T.StructName (_su, fullname) ->
          let (_su2, def) = Hashtbl.find env.structs_ fullname in
          (try 
             let e = 
               unsugar_anon_structure_element env e0
                 ({ e with e = RecordAccess (e, fldname); e_type = t })
                 name def
             in
             res |> Stack_.push e
           with Not_found -> ()
          )
        | _ -> raise (Impossible "checked anon elements are struct/union")
        )
      else ()
  );
  (match !res with
  | [x] -> x
  | [] -> raise Not_found
  | _x::_y::_xs ->
    raise (Error(E.Misc(spf "ambiguous unnamed structure element %s" name,
                               e.e_loc)))
  )
(*e: function [[Typecheck.unsugar_anon_structure_element]] *)

(*****************************************************************************)
(* AST Types to Types.t *)
(*****************************************************************************)
(*s: function [[Typecheck.type_]] *)
(* Expand typedefs and resolve constant expressions. *)
let rec type_ (env : env) (typ0 : typ) : Type.t =
  match typ0.t with
  | TBase t -> t
  | TPointer typ -> T.Pointer (type_ env typ)
  | TArray (eopt, typ) ->
      (match eopt with
      | None -> T.Array (None, type_ env typ)
      | Some e ->
        (try 
           let i = Eval_const.eval env.constants e in
           T.Array (Some i, type_ env typ)
         with Eval_const.NotAConstant ->
           raise (Error
                  (E.Misc("array size must be a positive constant",typ0.t_loc)))
        )
      )
  | TFunction (tret, (tparams, tdots)) ->
      T.Func (type_ env tret, 
                tparams |> List.map (fun p -> 
                  let t = type_ env p.p_type in
                  (match t with
                  (* libc.h has a 'typedef long jmp_buf[2]' and then functions
                   * like 'int setjmp(jmp_buf)' so we need to support that.
                   * less: could warn?
                   *)
                  | T.Array (_, t) -> T.Pointer t
                  (* stricter: could transform T.Func in pointer *)
                  | T.Func _ -> type_error t p.p_loc
                  | _ -> t
                  )
                ), tdots)
  | TStructName (su, fullname) -> T.StructName (su, fullname)
  (* expand enums! *)
  | TEnumName fullname -> T.I (Hashtbl.find env.enums fullname)
  (* expand typedefs! *)
  | TTypeName fullname -> Hashtbl.find env.typedefs fullname
(*e: function [[Typecheck.type_]] *)

(*****************************************************************************)
(* Expression typechecking *)
(*****************************************************************************)
(*s: function [[Typecheck.expr]] *)
let rec expr (env : env) (e0 : expr) : expr (* but type annotated *) =
  (* default env for recursive call *)
  let newenv = { env with expr_context = CtxWantValue } in

  match e0.e with
  (*s: [[Typecheck.expr()]] match [[e0.e]] cases *)
  | Sequence (e1, e2) -> 
    let e1 = expr newenv e1 in
    let e2 = expr newenv e2 in
    { e0 with e = Sequence (e1, e2); e_type = e2.e_type }
  (*x: [[Typecheck.expr()]] match [[e0.e]] cases *)
  | Int    (_s, inttype)   -> { e0 with e_type = T.I inttype }
  (*x: [[Typecheck.expr()]] match [[e0.e]] cases *)
  | Float  (_s, floattype) -> { e0 with e_type = T.F floattype }
  (*x: [[Typecheck.expr()]] match [[e0.e]] cases *)
  (* 5c: transformed in Id (ONAME), but better later *)
  | String (_s, t)         -> { e0 with e_type = t } |> array_to_pointer env
  (*x: [[Typecheck.expr()]] match [[e0.e]] cases *)
  | Id fullname ->
     if Hashtbl.mem env.constants fullname
     then
       let (i, inttype) = Hashtbl.find env.constants fullname in
       { e0 with e = Int (spf "%d" i, inttype); e_type = T.I inttype }
     else
       let idinfo = Hashtbl.find env.ids_ fullname in
       { e0 with e_type = idinfo.typ } |> array_to_pointer env
  (*x: [[Typecheck.expr()]] match [[e0.e]] cases *)

  | Binary (e1, op, e2) ->
    let e1 = expr newenv e1 in
    let e2 = expr newenv e2 in

    check_compatible_binary op e1.e_type e2.e_type e0.e_loc;

    (* todo: add casts if left and right not the same types? or do it later? *)
    let finalt : Type.t = 
      match op with

      | Arith Minus ->
        (match e1.e_type, e2.e_type with
        | T.Pointer _, T.Pointer _ -> T.long (* TODO? depend on Arch.t? *)
        | _ -> result_type_binary e1.e_type e2.e_type
        )

      | Arith (Plus | Mul | Div | Mod    
              | And | Or | Xor
              (* todo: also add T.int cast when shl/shr on right operand *)
              | ShiftLeft | ShiftRight
              ) -> 
        result_type_binary e1.e_type e2.e_type

      | Logical (Eq | NotEq  
                | Inf | Sup | InfEq | SupEq
                | AndLog | OrLog
                ) ->
        (* ugly: should be a T.Bool! C is ugly. *)
        T.int
    in
    (* TODO: add int cast for ShiftLeft, ShiftRight right operand 
     * TODO: call arith() to do "usual arithmetic conversions"? or do that
     * alt: later in Codegen? on in Rewrite.ml? 
     *)

    { e0 with e = Binary (e1, op, e2); e_type = finalt }
  (*x: [[Typecheck.expr()]] match [[e0.e]] cases *)
  | Unary (op, e) ->
    (match op with
    (* + E -~> 0 + E *)
    | UnPlus -> 
      let e = Binary ({e0 with e = Int ("0",(T.Int,T.Signed))}, Arith Plus,e) in
      expr env { e0 with e = e }
    (* - E -~> 0 - E *)
    | UnMinus -> 
      let e = Binary ({e0 with e = Int ("0",(T.Int,T.Signed))}, Arith Minus,e)in
      expr env { e0 with e = e }
    (* ~ E -~> -1 ^ E *)
    | Tilde ->
      let e = Binary ({e0 with e = Int ("-1",(T.Int,T.Signed))}, Arith Xor,e) in
      expr env { e0 with e = e }

    | Not ->
      let e = expr newenv e in
      (match e.e_type with
      (* what about T.Array? see array_to_pointer above *)
      | T.I _ | T.F _ | T.Pointer _ -> ()
      | _ -> type_error e.e_type e.e_loc 
      );
      { e0 with e = Unary (Not, e); e_type = T.int }
     (*s: [[Typecheck.expr()]] in [[Unary]], match [[op]] other cases *)
     | GetRef ->

         (* we dont want an additional '&' added before an array *)
         let e = expr { env with expr_context = CtxGetRef } e in

         if not (lvalue (e))
         then raise (Error (E.Misc ("not an l-value", e0.e_loc)));

         (* less: warn if take address of array or function, ADDROP *)
         { e0 with e = Unary (GetRef, e); 
                   e_type = T.Pointer (e.e_type) }
     (*x: [[Typecheck.expr()]] in [[Unary]], match [[op]] other cases *)
     | DeRef ->
         let e = expr newenv e in

         (match e.e_type with
         | T.Pointer t -> 
           { e0 with e = Unary (DeRef, e); 
                     e_type = t } |> array_to_pointer env
         (* what about T.Array? no need, see array_to_pointer() *)
         | _ -> type_error e.e_type e.e_loc
         )
     (*e: [[Typecheck.expr()]] in [[Unary]], match [[op]] other cases *)
     )
  (*x: [[Typecheck.expr()]] match [[e0.e]] cases *)
  | Assign (op, e1, e2) ->
    let e1 = expr newenv e1 in
    let e2 = expr newenv e2 in

    if not (lvalue (e1))
    then raise (Error (E.Misc ("not an l-value", e0.e_loc)));

    check_compatible_assign op e1.e_type e2.e_type e0.e_loc;

    (* todo: add cast on e2 if not same type,
     * todo: mixedasop thing?
     *)
    { e0 with e = Assign (op, e1, e2); e_type = e1.e_type }
  (*x: [[Typecheck.expr()]] match [[e0.e]] cases *)
  (* x[y] --> *(x+y), pointer arithmetic power *)
  | ArrayAccess (e1, e2) ->
    let e = Unary (DeRef, { e0 with e = Binary (e1, Arith Plus, e2) }) in
    expr env { e0 with e = e }
  (*x: [[Typecheck.expr()]] match [[e0.e]] cases *)
  | RecordAccess (e, name) ->
    let e = expr newenv e in

    (match e.e_type with
    | T.StructName (_su, fullname) ->
      let (_su2, def) = Hashtbl.find env.structs_ fullname in
      (try
         let t = List.assoc name def in
         { e0 with e = RecordAccess (e, name);
                   e_type = t } |> array_to_pointer env
       with Not_found ->
         (*s: [[Typecheck.expr()]] when field name not found and gensymed field exn *)
         if def |> List.exists (fun (fld, _) -> Ast.is_gensymed fld)
         then 
           try 
             unsugar_anon_structure_element env e0 e name def 
               |>array_to_pointer env
           with Not_found ->
             raise (Error(E.Misc(spf "not a member of struct/union: %s" name,
                                 e.e_loc)))
         (*e: [[Typecheck.expr()]] when field name not found and gensymed field exn *)
         else 
           raise (Error(E.Misc(spf "not a member of struct/union: %s" name,
                        e.e_loc)))
      )
    | _ -> type_error e.e_type e.e_loc
    )
  (*x: [[Typecheck.expr()]] match [[e0.e]] cases *)
  (* x->y --> ( *x).y *)
  | RecordPtAccess (e, name) ->
    let e = RecordAccess ({ e0 with e = Unary (DeRef, e)}, name) in
    expr env { e0 with e = e }
  (*x: [[Typecheck.expr()]] match [[e0.e]] cases *)
  | Call (e, es) ->
    (* less: should disable implicit OADDR for function here in env *)
    let e = expr newenv e in

    (match e.e_type with
    | T.Func (tret, tparams, varargs) ->
       (* we enable GetRef for array here (and functions) 
        * TODO? why not use newenv?
        *)
      let es = List.map (expr { env with expr_context = CtxWantValue }) es in

      check_args_vs_params es tparams varargs e0.e_loc;

      (* todo: add cast *)
      (* less: format checking *)
      { e0 with e = Call (e, es); 
                e_type = tret }

    | T.Pointer (T.Func (_tret, _tparams, _varargs)) ->
      (* stricter?: we could forbid it, but annoying for my print in libc.h *)
      let e = { e with e = Unary (DeRef, e); } in
      expr newenv { e0 with e = Call (e, es) }

    | _ -> type_error e.e_type e.e_loc
    )
  (*x: [[Typecheck.expr()]] match [[e0.e]] cases *)
  | Cast (typ, e) ->
    (* todo? set special env ADDROF|CASTOF? CASTOF seemed unused *)

    let t = type_ env typ in
    let e = expr newenv e in

    (match e.e_type, t with
    | T.I _, (T.I _ | T.F _ | T.Pointer _ | T.Void)
    | T.F _, (T.I _ | T.F _ | T.Void)
    | T.Pointer _, (T.I _ | T.Pointer _ | T.Void)
      -> ()
    (* less: seems pretty useless *)
    | T.Void, T.Void -> ()
    (* less: seems pretty useless *)
    | T.StructName (su1, _) , T.StructName (su2, _) when su1 =*= su2 -> ()
    | T.StructName _, T.Void -> ()
    | _ -> type_error2 e.e_type t e0.e_loc
    );
    { e0 with e = Cast (typ, e); 
              e_type = t } (* |> array_to_pointer ? *)
  (*x: [[Typecheck.expr()]] match [[e0.e]] cases *)
  | CondExpr (e1, e2, e3) ->
    let e1 = expr newenv e1 in
    let e2 = expr newenv e2 in
    let e3 = expr newenv e3 in

    (* stricter? should enforce e1.e_type is a Bool *)
    check_compatible_binary (Logical Eq) e2.e_type e3.e_type e0.e_loc;

    (* todo: special nil handling? need? *)
    let finalt = result_type_binary e2.e_type e3.e_type in

    (* todo: add cast, and special nil handling *)
    { e0 with e = CondExpr (e1, e2, e3); 
              e_type = finalt }
  (*x: [[Typecheck.expr()]] match [[e0.e]] cases *)
  (* ocaml-light: | Postfix (e, op) | Prefix (op, e) *)
  | Postfix (_, _) | Prefix (_, _) ->
    let (e, op) =
        match e0.e with
        | Postfix (e, op) -> e, op
        | Prefix (op, e) -> e, op
        | _ -> raise (Impossible "pattern match only those cases")
    in
    let e = expr newenv e in

    if not (lvalue (e))
    then raise (Error (E.Misc ("not an l-value", e.e_loc)));

    check_compatible_binary (Arith Plus) e.e_type T.int e0.e_loc;

    (match e.e_type with
    | T.Pointer T.Void ->
      raise (Error (E.Misc ("inc/dec of a void pointer", e.e_loc)));
    | _ -> ()
    );

    { e0 with e = 
        (match e0.e with 
        | Postfix _ -> Postfix (e, op)
        | Prefix _ -> Prefix (op, e)
        | _ -> raise (Impossible "pattern match only those cases")
        ); 
        e_type = e.e_type }
  (*x: [[Typecheck.expr()]] match [[e0.e]] cases *)
  | SizeOf(te) ->

    (match te with
    | Left e ->
      (* we pass a special context because if t2 mentions an array, 
       * we want the size of the array, not the size of a pointer to an array
       *)
      let e = expr { env with expr_context = CtxSizeof  } e in
      { e0 with e = SizeOf (Left e); 
                e_type = T.int }

    (* todo: build a fake expression but with the right expanded type
     * so the codegen later does not have to redo the job of expanding
     * typedefs.
     *)
    | Right typ ->
      { e0 with e = SizeOf (Right typ); 
                e_type = T.int }
    )
  (*e: [[Typecheck.expr()]] match [[e0.e]] cases *)

  | ArrayInit _
  | RecordInit _
  | GccConstructor _
      -> raise Todo
(*e: function [[Typecheck.expr]] *)
(*s: function [[Typecheck.expropt]] *)
and expropt (env : env) (eopt : expr option) : expr option = 
    match eopt with
    | None -> None
    | Some e -> Some (expr env e)
(*e: function [[Typecheck.expropt]] *)

(*****************************************************************************)
(* Statement *)
(*****************************************************************************)
(*s: function [[Typecheck.stmt]] *)
(* The code below is boilerplate, mostly.
 * expr() should not do any side effect on the environment, so we can
 * call recursively in any order stmt() and expr() (including the
 * reverse order of evaluation of OCaml for arguments).
 *)
let rec stmt (env : env) (st0 : stmt) : stmt (* with exprs inside annotated *) =
  { st0 with s = 
    (match st0.s with
    (*s: [[Typecheck.stmt()]] match [[st0.s]] cases *)
    | ExprSt e -> ExprSt (expr env e)
    | Block xs -> Block (List.map (stmt env) xs)
    (*x: [[Typecheck.stmt()]] match [[st0.s]] cases *)
    (* stricter? should require Bool, not abuse pointer *)
    | While (e, st) -> 
      While (expr env e, stmt env st)
    | DoWhile (st, e) -> 
      DoWhile (stmt env st, expr env e)
    (*x: [[Typecheck.stmt()]] match [[st0.s]] cases *)
    | For (e1either, e2opt, e3opt, st) ->
      (* we may have to do side effects on the environment, so we process
       * e1either first
       *)
      let e1either = 
        (match e1either with 
        | Left e1opt -> Left (expropt env e1opt)
        | Right _decls -> raise Todo
        )
      in
      For (e1either, expropt env e2opt, expropt env e3opt, stmt env st)
    (*x: [[Typecheck.stmt()]] match [[st0.s]] cases *)
    | Continue -> Continue
    | Break -> Break
    (*x: [[Typecheck.stmt()]] match [[st0.s]] cases *)
    | Label (name, st) -> Label (name, stmt env st)
    | Goto name -> Goto name
    (*x: [[Typecheck.stmt()]] match [[st0.s]] cases *)
    | If (e, st1, st2) -> 
      let e = expr env e in

      (match e.e_type with
      (* ugly: no real bool type in C; abuse int, float, and worse pointers *)
      | T.I _ | T.F _ | T.Pointer _ -> ()
      (* stricter: error when does not typecheck, not just set null type on e *)
      (* TODO: list remaining cases explicitely *)
      | _ -> type_error e.e_type e.e_loc
      );
      If (e, stmt env st1, stmt env st2)
    (*x: [[Typecheck.stmt()]] match [[st0.s]] cases *)
    | Switch (e, xs) -> 
      let e = expr env e in

      (* ensure e is a number! not a pointer 
       * TODO? I originally accepted T.F _  but I think that was a bug as
       * 5c does not seem to allow it.
       *)
      (match e.e_type with
      | T.I _ -> ()
      | _ -> type_error e.e_type e.e_loc
      );
      (* TODO: do the 0:int - (0:int - x) rewrite? *)
      Switch (e, stmt env xs)
    (*x: [[Typecheck.stmt()]] match [[st0.s]] cases *)
    (* less: should enforce int expr? *)
    | Case (e, st) -> Case (expr env e, stmt env st)
    | Default st -> Default (stmt env st)
    (*x: [[Typecheck.stmt()]] match [[st0.s]] cases *)
    | Return eopt -> 
      Return 
        (match eopt with
        | None -> 
          if env.return_type =*= T.Void
          then None 
          (* stricter: error, not warn *)
          else raise (Error (E.Misc ("null return of a typed function", 
                                     st0.s_loc)))
        | Some e -> 
          let e = expr env e in
          check_compatible_assign Eq_ env.return_type e.e_type e.e_loc;
          (* todo: add cast *)
          Some e
        )
    (*x: [[Typecheck.stmt()]] match [[st0.s]] cases *)
    | Var { v_name = fullname; v_loc = loc; v_type = typ;
            v_storage = stoopt; v_init = eopt} -> 

      let t = type_ env typ in
      let ini = expropt env eopt in
      (match t with
      (* stricter: forbid nested prototypes *)
      | T.Func _ -> 
         raise (Error(E.Misc("prototypes inside functions are forbidden",loc)));
      | _ -> ()
      );
      let sto =
        match stoopt with
        | None -> S.Local
        (* stricter? forbid? confusing anyway to shadow locals *)
        | Some S.Extern ->
          raise(Error(E.Misc 
                     ("extern declaration inside functions are forbidden",loc)))
        | Some S.Static -> 
          raise Todo
        | Some S.Local -> 
          (* stricter: I warn at least *)
          Error.warn "useless auto keyword" loc;
          S.Local
        | Some (S.Global | S.Param) -> 
          raise (Impossible "global/param are not keywords")
      in

      (match ini with
      | None -> ()
      | Some e ->
        (* alt: convert the whole Var in an Assign like in 5c? *)
        (* less: no const checking for this assign *)
        check_compatible_assign Eq_ t e.e_type loc
        (* todo: add cast if not same type *)
      );
      Hashtbl.add env.ids_ fullname { typ = t; sto; ini; loc };

      Var { v_name = fullname; v_loc = loc; v_type = typ; v_storage = stoopt;
            v_init = ini }
    (*e: [[Typecheck.stmt()]] match [[st0.s]] cases *)
    )
  }
(*e: function [[Typecheck.stmt]] *)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)
(*s: function [[Typecheck.check_and_annotate_program]] *)
(* 5c: was called tcom *)
let check_and_annotate_program (prog: Ast.program) : typed_program =
  let (ast, _locs) = prog in

  let funcs = ref [] in

  (*s: function [[Typecheck.check_and_annotate_program.toplevel]] *)
  let toplevel (env : env) = function
    (*s: [[Typecheck.check_and_annotate_program.toplevel()]] cases *)
    | StructDef { su_kind=su; su_name=fullname; su_loc=loc; su_flds=flds }->

      Hashtbl.add env.structs_ fullname 
        (su, flds |> List.map 
            (fun {fld_name = name; fld_loc=_; fld_type = typ } ->
              let t = type_ env typ in

              (*s: [[Typecheck.check_and_annotate_program()]] if gensymed name *)
              (* kenccext: c99ext?:
               * less: if there are multiple anon structure elements, we 
               * could check eagerly if no ambiguous fields instead
               * of checking it when you use a field.
               *)
              (match Ast.is_gensymed name, t with
              | false, _ -> ()
              | true, T.StructName _ -> ()
              | true, _ -> 
                raise (Error (E.Misc 
                       ("unnamed structure element must be struct/union", loc)))
              );
              (*e: [[Typecheck.check_and_annotate_program()]] if gensymed name *)
              (name, t)
            )
        )
    (*x: [[Typecheck.check_and_annotate_program.toplevel()]] cases *)
    | EnumDef { enum_name = fullname; enum_loc = _loc; enum_constants = csts }
      ->
      (* stricter: no support for float enum constants *)
      let lastvalue = ref 0 in
      let maxt = ref (T.Int, T.Signed) in
      csts |> List.iter (fun 
        { ecst_name = fullname; ecst_loc = loc; ecst_value = eopt } ->
          (match eopt with
          | Some e ->
            (try 
               (* less: should also return an integer type *)
               let i = Eval_const.eval env.constants e in
               let t = (T.Int, T.Signed) in
               (* todo: maxt := max_types !maxt t; *)
               Hashtbl.add env.constants fullname (i, t);
               lastvalue := i;
             with Eval_const.NotAConstant ->
               raise (Error (E.Misc (spf "enum not a constant: %s"
                                            (unwrap fullname), loc)))
            )
          | None ->
            (* todo: curt *)
            let t = (T.Int ,T.Signed) in
            Hashtbl.add env.constants fullname (!lastvalue, t);
          );
          incr lastvalue
      );
      Hashtbl.add env.enums fullname !maxt
    (*x: [[Typecheck.check_and_annotate_program.toplevel()]] cases *)
    | TypeDef { typedef_name = fullname; typedef_loc = _loc; typedef_type =typ}->
      Hashtbl.add env.typedefs fullname (type_ env typ)
    (*x: [[Typecheck.check_and_annotate_program.toplevel()]] cases *)
    (* remember that VarDecl covers also prototypes *)
    | VarDecl { v_name = fullname; v_loc = loc; v_type = typ;
                v_storage = stoopt; v_init = eopt} ->
      let t = type_ env typ in
      let ini = expropt env eopt in

      (* step 0: typechecking initializer *)
      (match ini with
      | None -> ()
      | Some e ->
        (* less: no const checking for this assign *)
        check_compatible_assign Eq_ t e.e_type loc
      );

      (* step1: check for weird declarations *)
      (match t, ini, stoopt with
      | T.Func _, Some _, _ -> 
        raise (Error(E.Misc 
                     ("illegal initializer (only var can be initialized)",loc)))
      (* stricter: 5c says nothing, clang just warns *)
      | _, Some _, Some S.Extern ->
        raise (Error (E.Misc ("'extern' variable has an initializer", loc)))
      | _ -> ()
      );

      (try 
         (* step2: check for weird redeclarations *)
         let old = Hashtbl.find env.ids_ fullname in

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
             | Some x, None  -> Some x
             | None, Some x -> Some x
             | None, None -> None
             | Some _x, Some _y ->
               raise (Error (E.Inconsistent (
               spf "redefinition of '%s'" (unwrap fullname), loc,
               "previous definition is here", old.loc)))
           in
           (* check storage compatibility and compute final storage *)
           let finalsto = 
             merge_storage_toplevel (unwrap fullname) loc stoopt ini old in

           Hashtbl.replace env.ids_ fullname 
             {typ = finalt; sto = finalsto; loc = loc; ini = finalini }
       with Not_found ->
         let finalsto =
           match stoopt with
           | None -> S.Global
           | Some S.Extern -> S.Extern
           | Some S.Static -> S.Static
           | Some S.Local -> 
             raise (Error(E.Misc 
                          ("illegal storage class for file-scoped entity",loc)))
           | Some (S.Global | S.Param) -> 
             raise (Impossible "global or param are not keywords")
         in
         Hashtbl.add env.ids_ fullname 
           {typ = t; sto = finalsto; loc = loc; ini = ini }
      )
    (*x: [[Typecheck.check_and_annotate_program.toplevel()]] cases *)
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
      let ini = Some { e = Id fullname; e_loc = loc; e_type = T.Void } in

      (try 
         (* check for weird redeclarations *)
         let old = Hashtbl.find env.ids_ fullname in

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
             | Some x, None -> Some x
             | None, Some x -> Some x
             | None, None -> None
             | Some _x, Some _y ->
               raise (Error (E.Inconsistent (
               spf "redefinition of '%s'" (unwrap fullname), loc,
               "previous definition is here", old.loc)))
           in
           (* check storage compatibility and compute final storage *)
           let finalsto = 
             merge_storage_toplevel (unwrap fullname) loc stoopt ini old in

           Hashtbl.replace env.ids_ fullname 
             {typ = finalt; sto = finalsto; loc = loc; ini = finalini }
       with Not_found ->
         let finalsto =
           match stoopt with
           | None -> S.Global
           | Some S.Static -> S.Static
           (* different than for VarDecl here *)
           | Some S.Extern -> 
             raise (Error (E.Misc ("'extern' function with initializer", loc)))
           | Some S.Local -> 
             raise (Error(E.Misc 
                          ("illegal storage class for file-scoped entity",loc)))
           | Some (S.Global | S.Param) -> 
             raise (Impossible "global or param are not keywords")
         in
         Hashtbl.add env.ids_ fullname 
           {typ = t; sto = finalsto; loc = loc; ini = ini }
      );

      (* add params in environment before process st *)
      let (tret, (tparams, _dots)) = ftyp in
      tparams |> List.iter (fun p ->
        p.p_name |> Option.iter (fun fullname ->
          let t = type_ env p.p_type in
          (match t with
          (* stricter: 5c and clang says nothing, could convert in pointer *) 
          | T.Array _ | T.Func _ -> type_error t p.p_loc
          (* todo: convert small types to int? see paramconv? *)
          | _ -> ()
          );
          Hashtbl.add env.ids_ fullname 
            {typ = t; sto = S.Param; loc = loc; ini = None }
        )
      );
      (* the expressions inside the statements are now annontated with types *)
      let st = stmt { env with return_type = type_ env tret } st in

      funcs := { def with f_body = st }::!funcs;
    (*e: [[Typecheck.check_and_annotate_program.toplevel()]] cases *)
  in
  (*e: function [[Typecheck.check_and_annotate_program.toplevel]] *)
  (*s: [[Typecheck.check_and_annotate_program()]] set initial [[env]] *)
  let env = {
    ids_ = Hashtbl_.create ();
    structs_ = Hashtbl_.create ();

    typedefs = Hashtbl_.create ();
    enums = Hashtbl_.create ();
    constants = Hashtbl_.create ();
  
    return_type = T.Void;
    expr_context = CtxWantValue;
  }
  in
  (*e: [[Typecheck.check_and_annotate_program()]] set initial [[env]] *)

  ast |> List.iter (toplevel env);

  { ids = env.ids_; structs = env.structs_; funcs = List.rev !funcs }
(*e: function [[Typecheck.check_and_annotate_program]] *)
(*e: Typecheck.ml *)
