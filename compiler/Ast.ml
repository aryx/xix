(*s: Ast.ml *)
(* Copyright 2016 Yoann Padioleau, see copyright.txt *)
open Common

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* An Abstract Syntax Tree (AST) for C.
 * 
 * The AST below does not match exactly the source code; I do a few
 * simplications at parsing time:
 *  - no nested struct definitions; they are lifted to the toplevel and 
 *    a blockid is associated with the tag name to avoid name conflicts
 *  - no anonymous structure; an artificial name is gensym'ed.
 *  - no anonymous structure element; an artificial field name is gensym'ed
 *  - no mix of typedefs with variable declarations;
 *    again typedefs are lifted to the top
 *  - enums are also lifted to the top (and its constants are tagged with
 *    a blockid)
 * 
 * This AST is actually more a named AST (but not a typed AST). 
 * Indeed, in C, you can not separate completely the naming phase from parsing.
 * The grammar of C has an ambiguity with typedefs, so we need to keep track of 
 * typedefs and identifiers and their scope during parsing. It would be
 * redundant to do this work again in a separate naming phase, so I 
 * name and resolve the scope of identifiers at parsing time
 * (however I check for inconsistencies or redefinitions after parsing).
 * Moreover, because I lift up struct definitions, I also keep track
 * and resolve the scope of tags.
 * 
 * See also pfff/lang_c/parsing/ast_c.ml and pfff/lang_cpp/parsing/ast_cpp.ml
 * 
 * todo: 
 *  - have a (large) integer and (large) double for Int and Float?
 *    Int64.t? if unsigned long long constant? enough? overflow in int64_of_str?
 *)

(*****************************************************************************)
(* The AST related types *)
(*****************************************************************************)

(*s: type [[Ast.loc]] *)
(* global linenumber after preprocessing *)
type loc = Location_cpp.loc
(*e: type [[Ast.loc]] *)
[@@deriving show]

(* ------------------------------------------------------------------------- *)
(* Name *)
(* ------------------------------------------------------------------------- *)

(*s: type [[Ast.name]] *)
type name = string
(*e: type [[Ast.name]] *)
[@@deriving show]

(*s: type [[Ast.blockid]] *)
(* for scope *)
type blockid = int (* same than Type_.blockid, repeated here for clarity *)
(*e: type [[Ast.blockid]] *)
[@@deriving show]

(*s: type [[Ast.fullname]] *)
(* A fully resolved and scoped name. 
 *
 * 5c: uses a reference to a symbol in a symbol table to fully qualify a name.
 * Instead, I use a unique blockid and an external hash or environment that
 * maps this fullname to the appropriate information.
 * I think it offers a better separation of concerns.
 * 
 * 'name' below can be a gensym'ed name for anonymous struct/union/enum.
 *)
type fullname = name * blockid (* same than Type_.fullname *)
(*e: type [[Ast.fullname]] *)
[@@deriving show]

(*s: type [[Ast.idkind]] *)
(* Used in Globals.ml/Lexer.mll/Parser.mly to recognize typedef identifiers.
 * alt: could be moved in a separate Naming.ml, but not worth it for just two types.
 *)
type idkind =
  | IdIdent
  | IdTypedef
  | IdEnumConstant
(*e: type [[Ast.idkind]] *)

(*s: type [[Ast.tagkind]] *)
(* to manage the scope of tags *)
type tagkind =
  | TagStruct
  | TagUnion
  | TagEnum
(*e: type [[Ast.tagkind]] *)

(* ------------------------------------------------------------------------- *)
(* Types *)
(* ------------------------------------------------------------------------- *)
(*s: type [[Ast.typ]] *)
(* What are the differences between typ below and Type.t? 
 * - typedef expansion is not done here
 * - constant expressions are not resolved yet 
 *  (those expressions can involve enum constants which will be resolved later).
 * Again, I think it offers a better separation of concerns.
 * 
 * Note that 'type_' and 'expr' are mutually recursive (because of const_expr).
 * todo: qualifier type
 *)
type typ = {
  t: type_bis;
  t_loc: loc;
}
(*e: type [[Ast.typ]] *)
(*s: type [[Ast.type_bis]] *)
  and type_bis = 
  | TBase of Type.t (* only the basic stuff *)
  | TPointer of typ
  | TArray of const_expr option * typ
  | TFunction of function_type

  (* no StructDef here; they are lifted up; just StructName *)
  | TStructName of Type.struct_kind * fullname
  (* In C an enum is really like an int. However, we could do
   * extended checks at some point to do more strict type checking! 
   *)
  | TEnumName of fullname
  | TTypeName of fullname
(*e: type [[Ast.type_bis]] *)

(*s: type [[Ast.function_type]] *)
 and function_type = (typ * (parameter list * bool (* var args '...' *)))
(*e: type [[Ast.function_type]] *)

(*s: type [[Ast.parameter]] *)
  and parameter = {
    (* When part of a prototype, the name is not always mentionned, hence
     * the option below.
     * 
     * I use 'fullname' here for consistency; parameters are treated like,
     * locals, so we can have below simply 'Id of fullname' and have 
     * no differences between accessing a local or a parameter.
     *)
    p_name: fullname option;
    p_loc: loc;

    p_type: typ;
  }
(*e: type [[Ast.parameter]] *)

(* ------------------------------------------------------------------------- *)
(* Expression *)
(* ------------------------------------------------------------------------- *)
(*s: type [[Ast.expr]] *)
and expr = { 
  e: expr_bis;
  e_loc: loc;
  (*s: [[Ast.expr]] other fields *)
  (* properly set during typechecking in typecheck.ml *)
  e_type: Type.t;
  (*e: [[Ast.expr]] other fields *)
}
(*e: type [[Ast.expr]] *)
(*s: type [[Ast.expr_bis]] *)
  and expr_bis = 
  (* Note that characters are transformed in Int at parsing time; no need Char *)
  | Int of string * Type.integer_type
  | Float of string * Type.float_type
  (* codegen: converted to Id after typechecking *)
  | String of string * Type.t (* always array of chars for now, no unicode *)

  (* Global, local, parameter, enum constant (can be scoped), function.
   * Not that the storage, type, usage of ids is computed later and stored
   * in external hashtbl.
   * codegen: Id converted to Int when the fullname refers to a enum constant 
   *)
  | Id of fullname

  | Call of expr * argument list

  (* should be a statement really *)
  | Assign of assignOp * expr * expr

  (* codegen: converted to pointer arithmetic, *(x+y) *)
  | ArrayAccess of expr * expr (* x[y] *)
  (* codegen: converted to pointer offset access *)
  | RecordAccess of expr * name (* x.y *)
  (* codegen: converted to RecordAccess, ( *x ).y *)
  | RecordPtAccess of expr * name (* x->y,  and not x.y!! *)

  (* less: bool (* explicit cast (xcast) *) *)
  | Cast of typ * expr

  | Postfix of expr * fixOp
  | Prefix of fixOp * expr
  (* contains GetRef and Deref!! pointers!  *)
  | Unary of unaryOp * expr
  | Binary of expr * binaryOp * expr

  | CondExpr of expr * expr * expr
  (* 'x, y', but really should be a statement, and could be removed.
   * I think mostly used in 'for(...;...;...)' 
   *)
  | Sequence of expr * expr

  (* codegen: converted to Int *)
  | SizeOf of (expr, typ) Either_.t

  (*s: [[Ast.expr]] initialiser cases *)
  (* should appear only in a variable initializer, or after GccConstructor *)
  | ArrayInit of (const_expr option * expr) list
  | RecordInit of (name * expr) list
  (*e: [[Ast.expr]] initialiser cases *)
  (*s: [[Ast.expr]] extension cases *)
  (* gccext: kenccext: *)
  | GccConstructor  of typ * expr (* always an ArrayInit (or RecordInit?) *)
  (*e: [[Ast.expr]] extension cases *)
(*e: type [[Ast.expr_bis]] *)

(*s: type [[Ast.argument]] *)
and argument = expr
(*e: type [[Ast.argument]] *)

(*s: type [[Ast.const_expr]] *)
(* Because we call the preprocessor first, the remaining cases
 * where const_expr is not a constant are basic arithmetic expressions
 * like 2 < < 3, or enum constants.
 *)
and const_expr = expr
(*e: type [[Ast.const_expr]] *)

(*s: type [[Ast.unaryOp]] *)
  and unaryOp  = 
    (* less: could be lifted up; those are really important operators *)
    | GetRef | DeRef 
    (* codegen: converted to binary operation with 0 (-x => 0-x) *)
    | UnPlus |  UnMinus 
    (* codegen: converted to -1 ^ x *)
    | Tilde 
    | Not 
(*e: type [[Ast.unaryOp]] *)
(*s: type [[Ast.assignOp]] *)
  and assignOp = Eq_ | OpAssign of arithOp
(*e: type [[Ast.assignOp]] *)
(*s: type [[Ast.fixOp]] *)
  and fixOp    = Dec | Inc
(*e: type [[Ast.fixOp]] *)

(*s: type [[Ast.binaryOp]] *)
  and binaryOp = Arith of arithOp | Logical of logicalOp
(*e: type [[Ast.binaryOp]] *)
(*s: type [[Ast.arithOp]] *)
       and arithOp   = 
         | Plus | Minus 
         | Mul | Div | Mod
         | ShiftLeft | ShiftRight 
         | And | Or | Xor
(*e: type [[Ast.arithOp]] *)
(*s: type [[Ast.logicalOp]] *)
       and logicalOp = 
         | Inf | Sup | InfEq | SupEq 
         | Eq | NotEq 
         | AndLog | OrLog
(*e: type [[Ast.logicalOp]] *)
[@@deriving show { with_path = false }]

(* ------------------------------------------------------------------------- *)
(* Statement *)
(* ------------------------------------------------------------------------- *)
(*s: type [[Ast.stmt]] *)
type stmt = {
  s: stmt_bis;
  s_loc: loc;
}
(*e: type [[Ast.stmt]] *)
(*s: type [[Ast.stmt_bis]] *)
  and stmt_bis = 
  | ExprSt of expr
  (* empty statement is simply Block [] *)
  | Block of stmt list

  (* else is optional and represented as an empty Block *)
  | If of expr * stmt * stmt

  (* expr must have an integer type; it can not be a pointer like in a If *)
  | Switch of expr * case_list

  | While of expr * stmt
  | DoWhile of stmt * expr
  | For of (expr option, var_decl list) Either_.t * 
           expr option * 
           expr option * 
           stmt

  | Return of expr option
  (* no argument to continue or break as in PHP *)
  | Continue | Break

  (* labels have a function scope, so no need to use 'fullname' here *)
  | Label of name * stmt
  | Goto of name

  (* should occur only in Switch *)
  | Case of expr * stmt
  | Default of stmt

  | Var of var_decl
(*e: type [[Ast.stmt_bis]] *)

(*s: type [[Ast.case_list]] *)
(* Can we have a specific case type? It is hard in C because they mix labels
 * and 'case' a lot (see the code in the lexer of 5c).
 *)
and case_list = stmt
(*e: type [[Ast.case_list]] *)

(* ------------------------------------------------------------------------- *)
(* Variables *)
(* ------------------------------------------------------------------------- *)

(*s: type [[Ast.var_decl]] *)
and var_decl = {
  v_name: fullname;
  v_loc: loc;
  v_storage: Storage.t option;
  v_type: typ;
  v_init: initialiser option;
}
(*e: type [[Ast.var_decl]] *)
(*s: type [[Ast.initialiser]] *)
 (* can have ArrayInit and RecordInit here in addition to other expr *)
 and initialiser = expr
(*e: type [[Ast.initialiser]] *)
[@@deriving show { with_path = false }]

(* ------------------------------------------------------------------------- *)
(* Definitions *)
(* ------------------------------------------------------------------------- *)
(*s: type [[Ast.func_def]] *)
type func_def = {
  (* functions have a global scope; no need for fullname here *)
  f_name: name;
  f_loc: loc;
  (* everything except Param or Auto *)
  f_storage: Storage.t option;
  f_type: function_type;
  (* always a Block *)
  f_body: stmt;
}
(*e: type [[Ast.func_def]] *)
[@@deriving show { with_path = false } ]

(*s: type [[Ast.struct_def]] *)
(* struct and union *)
type struct_def = {
  su_name: fullname;
  su_loc: loc;
  su_kind: Type.struct_kind;
  (* todo: bitfield annotation *)
  su_flds: field_def list;
}
(*e: type [[Ast.struct_def]] *)
(*s: type [[Ast.field_def]] *)
  (* Not the same than var_decl; fields have no storage and can have bitflds.*)
  and field_def = { 
   (* kenccext: anonymous structure element get an artificial field name
    * (see is_gensymed()) 
    *)
    fld_name: name;
    fld_loc: loc;
    fld_type: typ;
  }
(*e: type [[Ast.field_def]] *)
[@@deriving show { with_path = false } ]

(*s: type [[Ast.enum_def]] *)
type enum_def = { 
  (* this name is rarely used; C programmers rarely write 'enum Foo x;' *)
  enum_name: fullname;
  enum_loc: loc;
  enum_constants: enum_constant list;
}
(*e: type [[Ast.enum_def]] *)
(*s: type [[Ast.enum_constant]] *)
  and enum_constant = {
  (* we also need to use 'fullname' for constants, to scope them *)
    ecst_name: fullname;
    ecst_loc: loc;
    ecst_value: const_expr option;
  }
(*e: type [[Ast.enum_constant]] *)
[@@deriving show { with_path = false }]

(*s: type [[Ast.type_def]] *)
type type_def = { 
  typedef_name: fullname;
  typedef_loc: loc;
  typedef_type: typ;
}
[@@deriving show { with_path = false } ]
(*e: type [[Ast.type_def]] *)

(* ------------------------------------------------------------------------- *)
(* Program *)
(* ------------------------------------------------------------------------- *)
(*s: type [[Ast.toplevel]] *)
type toplevel =
  | StructDef of struct_def
  | TypeDef of type_def
  | EnumDef of enum_def
  (* globals, but also extern decls and prototypes *)
  | VarDecl of var_decl
  | FuncDef of func_def
(*e: type [[Ast.toplevel]] *)
[@@deriving show { with_path = false }]

(*s: type [[Ast.toplevels]] *)
type toplevels = toplevel list
(*e: type [[Ast.toplevels]] *)
[@@deriving show]

(*s: type [[Ast.program]] *)
type program = toplevels * Location_cpp.location_history list
(*e: type [[Ast.program]] *)
[@@deriving show]

(* ------------------------------------------------------------------------- *)
(* Any *)
(* ------------------------------------------------------------------------- *)
(*s: type [[Ast.any]] *)
(* for visitor and dumper *)
type any =
  | Expr of expr
  | Stmt of stmt
  | Type of typ
  | Toplevel of toplevel
  | Program of program
  | FinalType of Type.t
[@@deriving show { with_path = false } ]
(*e: type [[Ast.any]] *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
(*s: function [[Ast.tagkind_of_su]] *)
let tagkind_of_su = function
  | Type.Struct -> TagStruct
  | Type.Union -> TagUnion
(*e: function [[Ast.tagkind_of_su]] *)

(*s: function [[Ast.unwrap]] *)
let unwrap (name, _) = name    
(*e: function [[Ast.unwrap]] *)

open Common
open Regexp_.Operators
(*s: function [[Ast.is_gensymed]] *)
(* see also Parser.gensym *)
let is_gensymed str = 
  str =~ "|sym[0-9]+|.*"
(*e: function [[Ast.is_gensymed]] *)
(*e: Ast.ml *)
