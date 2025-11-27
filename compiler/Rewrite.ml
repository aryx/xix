(*s: Rewrite.ml *)
(* Copyright 2025 Yoann Padioleau, see copyright.txt *)
open Common
open Ast

module E = Check

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* This module does many things:
 *  - TODO convert String in Id
 *  - sanity check constructs have been transformed
 *    correctly in the typechecker
 *  - sanity check each expression has a type (not the default T.Void)
 *
 * todo mandatory:
 *  - pointer arithmetic
 *  - automatic casts
 * todo for opti?:
 *  - OADDR/OIND simplifications
 *  - put constants on the right for commutative operations
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)
type env = {
  foo: unit;
}

type error = Check.error
let string_of_error err =
  Check.string_of_error err
exception Error of error

let error msg loc =
  (* less: dump t? *)
  raise (Error (E.Misc (msg, loc)))

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(*e: Rewrite.ml *)
let map_option f env x = 
  Option.map (f env) x
let map_list f env x = 
  List.map (f env) x
let map_bool _env x = x
let map_string _env x = x

(*****************************************************************************)
(* Boilerplate visitor *)
(*****************************************************************************)

(* TODO *)
let map_type (_env : env) (t : Type.t) : Type.t =
  t

let map_loc _env v = v

let map_name _env (v : string) = v

let map_blockid _env (v : int) = v

let map_fullname env (v1, v2) =
  let v1 = map_name env v1 in
  let v2 = map_blockid env v2 in
  (v1, v2)


let rec map_typ env v =
  let {t; t_loc} = v in
  let t = map_type_bis env t in
  let t_loc = map_loc env t_loc in
  {t; t_loc}

and map_type_bis env v =
  match v with
  | TBase v ->
      (* TODO: let v = Type.map_t env v in *)
      TBase v
  | TPointer v ->
      let v = map_typ env v in
      TPointer v
  | TArray (v1, v2) ->
      let v1 = (map_option map_const_expr) env v1 in
      let v2 = map_typ env v2 in
      TArray (v1, v2)
  | TFunction v ->
      let v = map_function_type env v in
      TFunction v
  | TStructName (v1, v2) ->
      (* TODO: let v1 = Type.map_struct_kind env v1 in *)
      let v2 = map_fullname env v2 in
      TStructName (v1, v2)
  | TEnumName v ->
      let v = map_fullname env v in
      TEnumName v
  | TTypeName v ->
      let v = map_fullname env v in
      TTypeName v

and map_function_type env (v1, v2) =
  let v1 = map_typ env v1 in
  let map_tuple env (v1, v2) =
    let v1 = (map_list map_parameter) env v1 in
    let v2 = map_bool env v2 in
    (v1, v2)
  in
  let v2 = map_tuple env v2 in
  (v1, v2)

and map_parameter env v =
  let {p_name; p_loc; p_type} = v in
  let p_name = (map_option map_fullname) env p_name in
  let p_loc = map_loc env p_loc in
  let p_type = map_typ env p_type in
  {p_name; p_loc; p_type}




and map_expr env v =
  let {e; e_loc; e_type} = v in
  let e = map_expr_bis env e_loc e in
  let e_loc = map_loc env e_loc in
  let e_type = map_type env e_type in
  (match e, e_type with
  | Call _, Type.Void -> ()
  | _, Type.Void -> error "Impossible: got void type" e_loc
  | _ -> ()
  );
  {e; e_loc; e_type}

and map_expr_bis env loc v =
  match v with
  | Int (v1, v2) ->
      let v1 = map_string env v1 in
      (* TODO let v2 = Type.map_integer_type env v2 in *)
      Int (v1, v2)
  | Float (v1, v2) ->
      let v1 = map_string env v1 in
      (* TODO let v2 = Type.map_float_type env v2 in *)
      Float (v1, v2)
  | String (v1, v2) ->
      let v1 = map_string env v1 in
      (* TODO let v2 = Type.map_t env v2 in *)
      String (v1, v2)
  | Id v ->
      let v = map_fullname env v in
      Id v
  | Call (v1, v2) ->
      let v1 = map_expr env v1 in
      let v2 = (map_list map_argument) env v2 in
      Call (v1, v2)
  | Assign (v1, v2, v3) ->
      let v1 = map_assignOp env v1 in
      let v2 = map_expr env v2 in
      let v3 = map_expr env v3 in
      Assign (v1, v2, v3)
  | ArrayAccess (_v1, _v2) ->
      error "Impossible: ArrayAccess removed in Typecheck.ml" loc
  | RecordAccess (v1, v2) ->
      let v1 = map_expr env v1 in
      let v2 = map_name env v2 in
      RecordAccess (v1, v2)
  | RecordPtAccess (_v1, _v2) ->
      error "Impossible: RecordPtAccess removed in Typecheck.ml" loc
  | Cast (v1, v2) ->
      let v1 = map_typ env v1 in
      let v2 = map_expr env v2 in
      Cast (v1, v2)
  | Postfix (v1, v2) ->
      let v1 = map_expr env v1 in
      let v2 = map_fixOp env v2 in
      Postfix (v1, v2)
  | Prefix (v1, v2) ->
      let v1 = map_fixOp env v1 in
      let v2 = map_expr env v2 in
      Prefix (v1, v2)
  | Unary (v1, v2) ->
      (match v1 with
      | UnPlus | UnMinus | Tilde ->
          error "Impossible: unary +/-/~ removed in Typecheck.ml" loc
      | Not | GetRef | DeRef  -> ()
      );
      let v1 = map_unaryOp env v1 in
      let v2 = map_expr env v2 in
      Unary (v1, v2)
  | Binary (v1, v2, v3) ->
      let v1 = map_expr env v1 in
      let v2 = map_binaryOp env v2 in
      let v3 = map_expr env v3 in
      Binary (v1, v2, v3)
  | CondExpr (v1, v2, v3) ->
      let v1 = map_expr env v1 in
      let v2 = map_expr env v2 in
      let v3 = map_expr env v3 in
      CondExpr (v1, v2, v3)
  | Sequence (v1, v2) ->
      let v1 = map_expr env v1 in
      let v2 = map_expr env v2 in
      Sequence (v1, v2)
  | SizeOf v ->
    (* TODO match v with **)
     SizeOf v
  | ArrayInit v ->
      let map_tuple env (v1, v2) =
        let v1 = (map_option map_const_expr) env v1 in
        let v2 = map_expr env v2 in
        (v1, v2)
      in
      let v = (map_list map_tuple) env v in
      ArrayInit v
  | RecordInit v ->
      let map_tuple env (v1, v2) =
        let v1 = map_name env v1 in
        let v2 = map_expr env v2 in
        (v1, v2)
      in
      let v = (map_list map_tuple) env v in
      RecordInit v
  | GccConstructor (v1, v2) ->
      let v1 = map_typ env v1 in
      let v2 = map_expr env v2 in
      GccConstructor (v1, v2)

and map_argument env v =
  map_expr env v

and map_const_expr env v =
  map_expr env v

and map_unaryOp _env v = v

and map_assignOp _env v = v

and map_fixOp _env v = v

and map_binaryOp _env v = v

and _map_arithOp _env v = v

and _map_logicalOp _env v = v


let rec map_stmt env v =
  let {s; s_loc} = v in
  let s = map_stmt_bis env s in
  let s_loc = map_loc env s_loc in
  {s; s_loc}

and map_stmt_bis env v =
  match v with
  | ExprSt v ->
      let v = map_expr env v in
      ExprSt v
  | Block v ->
      let v = (map_list map_stmt) env v in
      Block v
  | If (v1, v2, v3) ->
      let v1 = map_expr env v1 in
      let v2 = map_stmt env v2 in
      let v3 = map_stmt env v3 in
      If (v1, v2, v3)
  | Switch (v1, v2) ->
      let v1 = map_expr env v1 in
      let v2 = map_case_list env v2 in
      Switch (v1, v2)
  | While (v1, v2) ->
      let v1 = map_expr env v1 in
      let v2 = map_stmt env v2 in
      While (v1, v2)
  | DoWhile (v1, v2) ->
      let v1 = map_stmt env v1 in
      let v2 = map_expr env v2 in
      DoWhile (v1, v2)
  | For (v1, v2, v3, v4) ->
      (* TODO match v1  *)
      let v2 = (map_option map_expr) env v2 in
      let v3 = (map_option map_expr) env v3 in
      let v4 = map_stmt env v4 in
      For (v1, v2, v3, v4)
  | Return v ->
      let v = (map_option map_expr) env v in
      Return v
  | Continue ->
      Continue
  | Break ->
      Break
  | Label (v1, v2) ->
      let v1 = map_name env v1 in
      let v2 = map_stmt env v2 in
      Label (v1, v2)
  | Goto v ->
      let v = map_name env v in
      Goto v
  | Case (v1, v2) ->
      let v1 = map_expr env v1 in
      let v2 = map_stmt env v2 in
      Case (v1, v2)
  | Default v ->
      let v = map_stmt env v in
      Default v
  | Var v ->
      let v = map_var_decl env v in
      Var v

and map_case_list env v =
  map_stmt env v

and map_var_decl env v =
  let {v_name; v_loc; v_storage; v_type; v_init} = v in
  let v_name = map_fullname env v_name in
  let v_loc = map_loc env v_loc in
  (* TODO let v_storage = (map_option Storage.map_t) env v_storage in *)
  let v_type = map_typ env v_type in
  let v_init = (map_option map_initialiser) env v_init in
  {v_name; v_loc; v_storage; v_type; v_init}

and map_initialiser env v =
  map_expr env v

let map_func_def env v =
  let {f_name; f_loc; f_storage; f_type; f_body} = v in
  let f_name = map_name env f_name in
  let f_loc = map_loc env f_loc in
  (* TODO let f_storage = (map_option Storage.map_t) env f_storage in *)
  let f_type = map_function_type env f_type in
  let f_body = map_stmt env f_body in
  {f_name; f_loc; f_storage; f_type; f_body}

(*
let rec map_struct_def env v =
  let {su_name; su_loc; su_kind; su_flds} = v in
  let su_name = map_fullname env su_name in
  let su_loc = map_loc env su_loc in
  (* TODO let su_kind = Type.map_struct_kind env su_kind in *)
  let su_flds = (map_list map_field_def) env su_flds in
  {su_name; su_loc; su_kind; su_flds}

and map_field_def env v =
  let {fld_name; fld_loc; fld_type} = v in
  let fld_name = map_name env fld_name in
  let fld_loc = map_loc env fld_loc in
  let fld_type = map_typ env fld_type in
  {fld_name; fld_loc; fld_type}

let rec map_enum_def env v =
  let {enum_name; enum_loc; enum_constants} = v in
  let enum_name = map_fullname env enum_name in
  let enum_loc = map_loc env enum_loc in
  let enum_constants = (map_list map_enum_constant) env enum_constants in
  {enum_name; enum_loc; enum_constants}

and map_enum_constant env v =
  let {ecst_name; ecst_loc; ecst_value} = v in
  let ecst_name = map_fullname env ecst_name in
  let ecst_loc = map_loc env ecst_loc in
  let ecst_value = (map_option map_const_expr) env ecst_value in
  {ecst_name; ecst_loc; ecst_value}

let map_type_def env v =
  let {typedef_name; typedef_loc; typedef_type} = v in
  let typedef_name = map_fullname env typedef_name in
  let typedef_loc = map_loc env typedef_loc in
  let typedef_type = map_typ env typedef_type in
  {typedef_name; typedef_loc; typedef_type}

let map_toplevel env v =
  match v with
  | StructDef v ->
      let v = map_struct_def env v in
      todo env v
  | TypeDef v ->
      let v = map_type_def env v in
      todo env v
  | EnumDef v ->
      let v = map_enum_def env v in
      todo env v
  | VarDecl v ->
      let v = map_var_decl env v in
      todo env v
  | FuncDef v ->
      let v = map_func_def env v in
      todo env v

let map_toplevels env v =
  (map_list map_toplevel) env v

let _map_program env (v1, v2) =
  let v1 = map_toplevels env v1 in
  (* TODO let v2 = (map_list Location_cpp.map_location_history) env v2 in *)
  todo env (v1, v2)

*)
(*
let rec map_any env v =
  match v with
  | Expr v ->
      let v = map_expr env v in
      todo env v
  | Stmt v ->
      let v = map_stmt env v in
      todo env v
  | Type v ->
      let v = map_typ env v in
      todo env v
  | Toplevel v ->
      let v = map_toplevel env v in
      todo env v
  | Program v ->
      let v = map_program env v in
      todo env v
  | FinalType v ->
      let v = Type.map_t env v in
      todo env v
*)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let rewrite (tast: Typecheck.typed_program) : Typecheck.typed_program =
  let env = { foo = () } in

  let funcs = tast.funcs |> List.map (map_func_def env) in
  { tast with Typecheck.funcs }
