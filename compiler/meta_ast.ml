(* generated by ocamltarzan with: camlp4o -o /tmp/yyy.ml -I pa/ pa_type_conv.cmo pa_vof.cmo  pr_o.cmo /tmp/xxx.ml  *)

open Ast

(* I modified vof_expr and vof_stmt to not always show the line information.
 * Less noise that way.
 *)
let show_all_pos = ref false

let vof_name x = Ocaml.vof_string x
let vof_blockid x = Ocaml.vof_int x
let vof_loc x = Ocaml.vof_int x
let vof_struct_kind x = Meta_type.vof_struct_kind x

module Type = Meta_type
module Storage = Meta_storage
module Common = Ocaml

let vof_fullname (v1, v2) =
  let v1 = vof_name v1 and v2 = vof_blockid v2 in Ocaml.VTuple [ v1; v2 ]


let rec vof_type_ { t = v_t; t_loc = v_t_loc } =
  if !show_all_pos
  then
  let bnds = [] in
  let arg = vof_loc v_t_loc in
  let bnd = ("t_loc", arg) in
  let bnds = bnd :: bnds in
  let arg = vof_type_bis v_t in
  let bnd = ("t", arg) in let bnds = bnd :: bnds in Ocaml.VDict bnds
  else vof_type_bis v_t
and vof_type_bis =
  function
  | TBase v1 -> let v1 = Type.vof_t v1 in Ocaml.VSum (("TBase", [ v1 ]))
  | TPointer v1 -> let v1 = vof_type_ v1 in Ocaml.VSum (("TPointer", [ v1 ]))
  | TArray ((v1, v2)) ->
      let v1 = Ocaml.vof_option vof_const_expr v1
      and v2 = vof_type_ v2
      in Ocaml.VSum (("TArray", [ v1; v2 ]))
  | TFunction v1 ->
      let v1 = vof_function_type v1 in Ocaml.VSum (("TFunction", [ v1 ]))
  | TStructName ((v1, v2)) ->
      let v1 = vof_struct_kind v1
      and v2 = vof_fullname v2
      in Ocaml.VSum (("TStructName", [ v1; v2 ]))
  | TEnumName v1 ->
      let v1 = vof_fullname v1 in Ocaml.VSum (("TEnumName", [ v1 ]))
  | TTypeName v1 ->
      let v1 = vof_fullname v1 in Ocaml.VSum (("TTypeName", [ v1 ]))
and vof_function_type (v1, v2) =
  let v1 = vof_type_ v1
  and v2 =
    match v2 with
    | (v1, v2) ->
        let v1 = Ocaml.vof_list vof_parameter v1
        and v2 = Ocaml.vof_bool v2
        in Ocaml.VTuple [ v1; v2 ]
  in Ocaml.VTuple [ v1; v2 ]
and vof_parameter { p_name = v_p_name; p_loc = v_p_loc; p_type = v_p_type } =
  let bnds = [] in
  let arg = vof_type_ v_p_type in
  let bnd = ("p_type", arg) in
  let bnds = bnd :: bnds in
  let arg = vof_loc v_p_loc in
  let bnd = ("p_loc", arg) in
  let bnds = bnd :: bnds in
  let arg = Ocaml.vof_option vof_fullname v_p_name in
  let bnd = ("p_name", arg) in let bnds = bnd :: bnds in Ocaml.VDict bnds

and vof_expr { e = v_e; e_loc = v_e_loc } =
  if !show_all_pos
  then
  let bnds = [] in
  let arg = vof_loc v_e_loc in
  let bnd = ("e_loc", arg) in
  let bnds = bnd :: bnds in
  let arg = vof_expr_bis v_e in
  let bnd = ("e", arg) in let bnds = bnd :: bnds in Ocaml.VDict bnds
  else vof_expr_bis v_e
and vof_expr_bis =
  function
  | Int ((v1, v2, v3)) ->
      let v1 = Ocaml.vof_string v1
      and v2 = Type.vof_sign v2
      and v3 = Storage.vof_intsize v3
      in Ocaml.VSum (("Int", [ v1; v2; v3 ]))
  | Float ((v1, v2)) ->
      let v1 = Ocaml.vof_string v1
      and v2 = Storage.vof_floatsize v2
      in Ocaml.VSum (("Float", [ v1; v2 ]))
  | String ((v1, v2)) ->
      let v1 = Ocaml.vof_string v1
      and v2 = Storage.vof_stringsize v2
      in Ocaml.VSum (("String", [ v1; v2 ]))
  | Id v1 -> let v1 = vof_fullname v1 in Ocaml.VSum (("Id", [ v1 ]))
  | Call ((v1, v2)) ->
      let v1 = vof_expr v1
      and v2 = Ocaml.vof_list vof_argument v2
      in Ocaml.VSum (("Call", [ v1; v2 ]))
  | Assign ((v1, v2, v3)) ->
      let v1 = vof_assignOp v1
      and v2 = vof_expr v2
      and v3 = vof_expr v3
      in Ocaml.VSum (("Assign", [ v1; v2; v3 ]))
  | ArrayAccess ((v1, v2)) ->
      let v1 = vof_expr v1
      and v2 = vof_expr v2
      in Ocaml.VSum (("ArrayAccess", [ v1; v2 ]))
  | RecordAccess ((v1, v2)) ->
      let v1 = vof_expr v1
      and v2 = vof_name v2
      in Ocaml.VSum (("RecordAccess", [ v1; v2 ]))
  | RecordPtAccess ((v1, v2)) ->
      let v1 = vof_expr v1
      and v2 = vof_name v2
      in Ocaml.VSum (("RecordPtAccess", [ v1; v2 ]))
  | Cast ((v1, v2)) ->
      let v1 = vof_type_ v1
      and v2 = vof_expr v2
      in Ocaml.VSum (("Cast", [ v1; v2 ]))
  | Postfix ((v1, v2)) ->
      let v1 = vof_expr v1
      and v2 = vof_fixOp v2
      in Ocaml.VSum (("Postfix", [ v1; v2 ]))
  | Prefix ((v1, v2)) ->
      let v1 = vof_fixOp v1
      and v2 = vof_expr v2
      in Ocaml.VSum (("Prefix", [ v1; v2 ]))
  | Unary ((v1, v2)) ->
      let v1 = vof_unaryOp v1
      and v2 = vof_expr v2
      in Ocaml.VSum (("Unary", [ v1; v2 ]))
  | Binary ((v1, v2, v3)) ->
      let v1 = vof_expr v1
      and v2 = vof_binaryOp v2
      and v3 = vof_expr v3
      in Ocaml.VSum (("Binary", [ v1; v2; v3 ]))
  | CondExpr ((v1, v2, v3)) ->
      let v1 = vof_expr v1
      and v2 = vof_expr v2
      and v3 = vof_expr v3
      in Ocaml.VSum (("CondExpr", [ v1; v2; v3 ]))
  | Sequence ((v1, v2)) ->
      let v1 = vof_expr v1
      and v2 = vof_expr v2
      in Ocaml.VSum (("Sequence", [ v1; v2 ]))
  | SizeOf v1 ->
      let v1 = Common.vof_either vof_expr vof_type_ v1
      in Ocaml.VSum (("SizeOf", [ v1 ]))
  | ArrayInit v1 ->
      let v1 =
        Ocaml.vof_list
          (fun (v1, v2) ->
             let v1 = Ocaml.vof_option vof_const_expr v1
             and v2 = vof_expr v2
             in Ocaml.VTuple [ v1; v2 ])
          v1
      in Ocaml.VSum (("ArrayInit", [ v1 ]))
  | RecordInit v1 ->
      let v1 =
        Ocaml.vof_list
          (fun (v1, v2) ->
             let v1 = vof_name v1
             and v2 = vof_expr v2
             in Ocaml.VTuple [ v1; v2 ])
          v1
      in Ocaml.VSum (("RecordInit", [ v1 ]))
  | GccConstructor ((v1, v2)) ->
      let v1 = vof_type_ v1
      and v2 = vof_expr v2
      in Ocaml.VSum (("GccConstructor", [ v1; v2 ]))
and vof_argument v = vof_expr v
and vof_const_expr v = vof_expr v
and vof_unaryOp =
  function
  | GetRef -> Ocaml.VSum (("GetRef", []))
  | DeRef -> Ocaml.VSum (("DeRef", []))
  | UnPlus -> Ocaml.VSum (("UnPlus", []))
  | UnMinus -> Ocaml.VSum (("UnMinus", []))
  | Tilde -> Ocaml.VSum (("Tilde", []))
  | Not -> Ocaml.VSum (("Not", []))
and vof_assignOp =
  function
  | SimpleAssign -> Ocaml.VSum (("SimpleAssign", []))
  | OpAssign v1 ->
      let v1 = vof_arithOp v1 in Ocaml.VSum (("OpAssign", [ v1 ]))
and vof_fixOp =
  function
  | Dec -> Ocaml.VSum (("Dec", []))
  | Inc -> Ocaml.VSum (("Inc", []))
and vof_binaryOp =
  function
  | Arith v1 -> let v1 = vof_arithOp v1 in Ocaml.VSum (("Arith", [ v1 ]))
  | Logical v1 ->
      let v1 = vof_logicalOp v1 in Ocaml.VSum (("Logical", [ v1 ]))
and vof_arithOp =
  function
  | Plus -> Ocaml.VSum (("Plus", []))
  | Minus -> Ocaml.VSum (("Minus", []))
  | Mul -> Ocaml.VSum (("Mul", []))
  | Div -> Ocaml.VSum (("Div", []))
  | Mod -> Ocaml.VSum (("Mod", []))
  | ShiftLeft -> Ocaml.VSum (("ShiftLeft", []))
  | ShiftRight -> Ocaml.VSum (("ShiftRight", []))
  | And -> Ocaml.VSum (("And", []))
  | Or -> Ocaml.VSum (("Or", []))
  | Xor -> Ocaml.VSum (("Xor", []))
and vof_logicalOp =
  function
  | Inf -> Ocaml.VSum (("Inf", []))
  | Sup -> Ocaml.VSum (("Sup", []))
  | InfEq -> Ocaml.VSum (("InfEq", []))
  | SupEq -> Ocaml.VSum (("SupEq", []))
  | Eq -> Ocaml.VSum (("Eq", []))
  | NotEq -> Ocaml.VSum (("NotEq", []))
  | AndLog -> Ocaml.VSum (("AndLog", []))
  | OrLog -> Ocaml.VSum (("OrLog", []))
  
let rec vof_stmt { st = v_s; stmt_loc = v_stmt_loc } =
  if !show_all_pos
  then
  let bnds = [] in
  let arg = vof_loc v_stmt_loc in
  let bnd = ("stmt_loc", arg) in
  let bnds = bnd :: bnds in
  let arg = vof_stmt_bis v_s in
  let bnd = ("st", arg) in let bnds = bnd :: bnds in Ocaml.VDict bnds
  else vof_stmt_bis v_s
and vof_stmt_bis =
  function
  | ExprSt v1 -> let v1 = vof_expr v1 in Ocaml.VSum (("ExprSt", [ v1 ]))
  | Block v1 ->
      let v1 = Ocaml.vof_list vof_stmt v1 in Ocaml.VSum (("Block", [ v1 ]))
  | If ((v1, v2, v3)) ->
      let v1 = vof_expr v1
      and v2 = vof_stmt v2
      and v3 = vof_stmt v3
      in Ocaml.VSum (("If", [ v1; v2; v3 ]))
  | Switch ((v1, v2)) ->
      let v1 = vof_expr v1
      and v2 = vof_caselist v2
      in Ocaml.VSum (("Switch", [ v1; v2 ]))
  | While ((v1, v2)) ->
      let v1 = vof_expr v1
      and v2 = vof_stmt v2
      in Ocaml.VSum (("While", [ v1; v2 ]))
  | DoWhile ((v1, v2)) ->
      let v1 = vof_stmt v1
      and v2 = vof_expr v2
      in Ocaml.VSum (("DoWhile", [ v1; v2 ]))
  | For ((v1, v2, v3, v4)) ->
      let v1 =
        Common.vof_either (Ocaml.vof_option vof_expr)
          (Ocaml.vof_list vof_var_decl) v1
      and v2 = Ocaml.vof_option vof_expr v2
      and v3 = Ocaml.vof_option vof_expr v3
      and v4 = vof_stmt v4
      in Ocaml.VSum (("For", [ v1; v2; v3; v4 ]))
  | Return v1 ->
      let v1 = Ocaml.vof_option vof_expr v1
      in Ocaml.VSum (("Return", [ v1 ]))
  | Continue -> Ocaml.VSum (("Continue", []))
  | Break -> Ocaml.VSum (("Break", []))
  | Label ((v1, v2)) ->
      let v1 = vof_name v1
      and v2 = vof_stmt v2
      in Ocaml.VSum (("Label", [ v1; v2 ]))
  | Goto v1 -> let v1 = vof_name v1 in Ocaml.VSum (("Goto", [ v1 ]))
  | Case ((v1, v2)) ->
      let v1 = vof_expr v1
      and v2 = vof_stmt v2
      in Ocaml.VSum (("Case", [ v1; v2 ]))
  | Default v1 -> let v1 = vof_stmt v1 in Ocaml.VSum (("Default", [ v1 ]))
  | Var v1 -> let v1 = vof_var_decl v1 in Ocaml.VSum (("Var", [ v1 ]))
and vof_caselist v = vof_stmt v
and
  vof_var_decl {
                 v_name = v_v_name;
                 v_loc = v_v_loc;
                 v_storage = v_v_storage;
                 v_type = v_v_type;
                 v_init = v_v_init
               } =
  let bnds = [] in
  let arg = Ocaml.vof_option vof_initialiser v_v_init in
  let bnd = ("v_init", arg) in
  let bnds = bnd :: bnds in
  let arg = vof_type_ v_v_type in
  let bnd = ("v_type", arg) in
  let bnds = bnd :: bnds in
  let arg = Ocaml.vof_option Storage.vof_t v_v_storage in
  let bnd = ("v_storage", arg) in
  let bnds = bnd :: bnds in
  let arg = vof_loc v_v_loc in
  let bnd = ("v_loc", arg) in
  let bnds = bnd :: bnds in
  let arg = vof_fullname v_v_name in
  let bnd = ("v_name", arg) in let bnds = bnd :: bnds in Ocaml.VDict bnds
and vof_initialiser v = vof_expr v
  
let vof_func_def {
                   f_name = v_f_name;
                   f_loc = v_f_loc;
                   f_storage = v_f_storage;
                   f_type = v_f_type;
                   f_body = v_f_body
                 } =
  let bnds = [] in
  let arg = vof_stmt v_f_body in
  let bnd = ("f_body", arg) in
  let bnds = bnd :: bnds in
  let arg = vof_function_type v_f_type in
  let bnd = ("f_type", arg) in
  let bnds = bnd :: bnds in
  let arg = Ocaml.vof_option Storage.vof_t v_f_storage in
  let bnd = ("f_storage", arg) in
  let bnds = bnd :: bnds in
  let arg = vof_loc v_f_loc in
  let bnd = ("f_loc", arg) in
  let bnds = bnd :: bnds in
  let arg = vof_name v_f_name in
  let bnd = ("f_name", arg) in let bnds = bnd :: bnds in Ocaml.VDict bnds

let rec
  vof_struct_def {
                   s_name = v_s_name;
                   s_loc = v_s_loc;
                   s_kind = v_s_kind;
                   s_flds = v_s_flds
                 } =
  let bnds = [] in
  let arg = Ocaml.vof_list vof_field_def v_s_flds in
  let bnd = ("s_flds", arg) in
  let bnds = bnd :: bnds in
  let arg = vof_struct_kind v_s_kind in
  let bnd = ("s_kind", arg) in
  let bnds = bnd :: bnds in
  let arg = vof_loc v_s_loc in
  let bnd = ("s_loc", arg) in
  let bnds = bnd :: bnds in
  let arg = vof_fullname v_s_name in
  let bnd = ("s_name", arg) in let bnds = bnd :: bnds in Ocaml.VDict bnds
  
and
  vof_field_def {
                  fld_name = v_fld_name;
                  fld_loc = v_fld_loc;
                  fld_type = v_fld_type
                } =
  let bnds = [] in
  let arg = vof_type_ v_fld_type in
  let bnd = ("fld_type", arg) in
  let bnds = bnd :: bnds in
  let arg = vof_loc v_fld_loc in
  let bnd = ("fld_loc", arg) in
  let bnds = bnd :: bnds in
  let arg = vof_name v_fld_name in
  let bnd = ("fld_name", arg) in let bnds = bnd :: bnds in Ocaml.VDict bnds
  
let rec
  vof_enum_def {
                 e_name = v_e_name;
                 e_loc = v_e_loc;
                 e_constants = v_e_constants
               } =
  let bnds = [] in
  let arg = Ocaml.vof_list vof_enum_constant v_e_constants in
  let bnd = ("e_constants", arg) in
  let bnds = bnd :: bnds in
  let arg = vof_loc v_e_loc in
  let bnd = ("e_loc", arg) in
  let bnds = bnd :: bnds in
  let arg = vof_fullname v_e_name in
  let bnd = ("e_name", arg) in let bnds = bnd :: bnds in Ocaml.VDict bnds


and
  vof_enum_constant {
                      ecst_name = v_ecst_name;
                      ecst_loc = v_ecst_loc;
                      ecst_value = v_ecst_value
                    } =
  let bnds = [] in
  let arg = Ocaml.vof_option vof_const_expr v_ecst_value in
  let bnd = ("ecst_value", arg) in
  let bnds = bnd :: bnds in
  let arg = vof_loc v_ecst_loc in
  let bnd = ("ecst_loc", arg) in
  let bnds = bnd :: bnds in
  let arg = vof_fullname v_ecst_name in
  let bnd = ("ecst_name", arg) in let bnds = bnd :: bnds in Ocaml.VDict bnds
  
let vof_type_def {
                   typedef_name = v_typedef_name;
                   typedef_loc = v_typedef_loc;
                   typedef_type = v_typedef_type
                 } =
  let bnds = [] in
  let arg = vof_type_ v_typedef_type in
  let bnd = ("typedef_type", arg) in
  let bnds = bnd :: bnds in
  let arg = vof_loc v_typedef_loc in
  let bnd = ("typedef_loc", arg) in
  let bnds = bnd :: bnds in
  let arg = vof_fullname v_typedef_name in
  let bnd = ("typedef_name", arg) in
  let bnds = bnd :: bnds in Ocaml.VDict bnds
  
let vof_toplevel =
  function
  | StructDef v1 ->
      let v1 = vof_struct_def v1 in Ocaml.VSum (("StructDef", [ v1 ]))
  | TypeDef v1 ->
      let v1 = vof_type_def v1 in Ocaml.VSum (("TypeDef", [ v1 ]))
  | EnumDef v1 ->
      let v1 = vof_enum_def v1 in Ocaml.VSum (("EnumDef", [ v1 ]))
  | FuncDef v1 ->
      let v1 = vof_func_def v1 in Ocaml.VSum (("FuncDef", [ v1 ]))
  | VarDecl v1 ->
      let v1 = vof_var_decl v1 in Ocaml.VSum (("VarDecl", [ v1 ]))
  
let vof_program v = Ocaml.vof_list vof_toplevel v
  
let vof_any =
  function
  | Expr v1 -> let v1 = vof_expr v1 in Ocaml.VSum (("Expr", [ v1 ]))
  | Stmt v1 -> let v1 = vof_stmt v1 in Ocaml.VSum (("Stmt", [ v1 ]))
  | Type v1 -> let v1 = vof_type_ v1 in Ocaml.VSum (("Type", [ v1 ]))
  | Toplevel v1 ->
      let v1 = vof_toplevel v1 in Ocaml.VSum (("Toplevel", [ v1 ]))
  | Program v1 -> let v1 = vof_program v1 in Ocaml.VSum (("Program", [ v1 ]))
