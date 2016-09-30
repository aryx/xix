(* Copyright 2016 Yoann Padioleau, see copyright.txt *)

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* An Abstract Syntax Tree (AST) for C.
 * 
 * The AST does not match exactly the source code; we do a few simplications
 * at parsing time:
 *  - no nested struct definitions; they are lifted to the toplevel and 
 *    a blockid is associated with the tag name to avoid name conflicts
 *  - no anonymous structure; an artificial name is gensym'ed.
 *  - no mix of typedefs with other variable declarations; again
 *    they are lifted to the top
 *  - enums are also lifted to the top (and its constants are tagged with
 *    a blockid)
 * 
 * See also pfff/lang_c/parsing/ast_c.ml and pfff/lang_cpp/parsing/ast_cpp.ml
 *)

(*****************************************************************************)
(* The AST related types *)
(*****************************************************************************)

(* global linenumber after preprocessing *)
type loc = Location_cpp.loc

(* ------------------------------------------------------------------------- *)
(* Name *)
(* ------------------------------------------------------------------------- *)

(* less: ref to a symbol? or use external hash? 
 * todo: lineno field?
 *)
type name = string

(* for scope *)
type blockid = int

(* name below can be a gensym'ed name for anonymous struct/union/enum *)
type fullname = name * blockid

(* Used in globals.ml/lexer.mll/parser.mly to recognize typedef identifiers. *) 
type idkind =
  | IdIdent
  | IdTypedef
  | IdEnumConstant

(* ------------------------------------------------------------------------- *)
(* Types *)
(* ------------------------------------------------------------------------- *)
(* Merge with Type.t? No. For one thing, typedef expansion is not done here.
 * Moreover constant expressions are also not resolved yet (those expressions
 * can involve enum constants which will be resolved later).
 * Better to separate I think.
 * todo: lineno field
 * todo: qualifier type
 *)
type type_ =
  | TBase of Type.t (* only the Basic stuff *)
  | TPointer of type_
  | TArray of const_expr option * type_
  | TFunction of function_type

  | TStructName of struct_kind * fullname
  (* In C an enum is really like an int. However, we could do
   * extended checks at some point to do more strict type checking! 
   *)
  | TEnumName of fullname
  | TTypeName of fullname

 and function_type = (type_ * (parameter list * bool (* var args *)))

  and parameter = {
    p_type: type_;
    (* when part of a prototype, the name is not always mentionned *)
    p_name: name option;
  }

 and struct_kind = Struct | Union

(* ------------------------------------------------------------------------- *)
(* Expression *)
(* ------------------------------------------------------------------------- *)
(* todo: lineno field *)
and expr =
  (* Note that characters are transformed in Int at parsing time; no need Char*)
  | Int of string * Storage.intsize
  | Float of string * Storage.floatsize
  | String of string * Storage.stringsize

  (* Global, local, parameter, enum constant (can be scoped), function *)
  | Id of fullname

  | Call of expr * argument list

  (* should be a statement really *)
  | Assign of assignOp * expr * expr

  | ArrayAccess of expr * expr (* x[y] *)
  (* Why x->y instead of x.y choice?
   * it's more consistent with ArrayAccess where expr has to be
   * a kind of pointer too. That means x.y is actually unsugared in (&x)->y
   *)
  | RecordPtAccess of expr * name (* x->y,  and not x.y!! *)

  | Cast of type_ * expr

  | Postfix of expr * fixOp
  | Infix of expr * fixOp
  (* contains GetRef and Deref!!  *)
  | Unary of expr * unaryOp
  | Binary of expr * binaryOp * expr

  | CondExpr of expr * expr * expr
  (* should be a statement ... *)
  | Sequence of expr * expr

  | SizeOf of (expr, type_) Common.either

  (* should appear only in a variable initializer, or after GccConstructor *)
  | ArrayInit of (expr option * expr) list
  | RecordInit of (name * expr) list
  (* gccext: kenccext: *)
  | GccConstructor  of type_ * expr (* always an ArrayInit (or RecordInit?) *)

  | ExprTodo

and argument = expr

(* Now that we call the preprocessor first, cases where const_expr is
 * not a constant? Can have basic arithmetic here? 
 * At least can have enum constants.
 *)
and const_expr = expr

  and unaryOp  = 
    (* less: could be lift up, those are really important operators *)
    | GetRef | DeRef 
    | UnPlus |  UnMinus | Tilde | Not 
  and assignOp = SimpleAssign | OpAssign of arithOp
  and fixOp    = Dec | Inc

  and binaryOp = Arith of arithOp | Logical of logicalOp
       and arithOp   = 
         | Plus | Minus 
         | Mul | Div | Mod
         | ShiftLeft | ShiftRight 
         | And | Or | Xor
       and logicalOp = 
         | Inf | Sup | InfEq | SupEq 
         | Eq | NotEq 
         | AndLog | OrLog

 (* with tarzan *)

(* ------------------------------------------------------------------------- *)
(* Statement *)
(* ------------------------------------------------------------------------- *)
(* todo: lineno field *)
type stmt =
  | ExprSt of expr
  | Block of stmt list

  | If of expr * stmt * stmt

  | Switch of expr * case list

  | While of expr * stmt
  | DoWhile of stmt * expr
  | For of (expr option, var_decl list) Common.either * 
           expr option * 
           expr option * 
           stmt

  | Return of expr option
  | Continue | Break

  | Label of name * stmt
  | Goto of name

  (* should occur only in Switch *)
  | Case of expr * stmt
  | Default of stmt

  | Vars of var_decl list

  | StmtTodo



(* Can we have a specific case type? It is hard in C because they mix labels
 * and 'case' a lot (see the code in the lexer of 5c).
 *)
and case = stmt

(* ------------------------------------------------------------------------- *)
(* Variables *)
(* ------------------------------------------------------------------------- *)

and var_decl = {
  v_name: fullname;
  v_type: type_;
  v_storage: Storage.t;
  v_init: initialiser option;
}
 (* can have ArrayInit and RecordInit here in addition to other expr *)
 and initialiser = expr
 (* with tarzan *)


(* ------------------------------------------------------------------------- *)
(* Definitions *)
(* ------------------------------------------------------------------------- *)
type func_def = {
  (* functions are globals, no need for fullname here *)
  f_name: name;
  f_type: function_type;
  (* always a Block *)
  f_body: stmt;
  (* not Param|Auto *)
  f_storage: Storage.t;
}
 (* with tarzan *)

type struct_def = {
  s_name: fullname;
  s_kind: struct_kind;
  s_flds: field_def list;
}

  (* We could merge with var_decl, but fields have no storage, and
   * they can have bitfields.
   *)
  and field_def = { 
   (* less: bitfield annotation
    * kenccext: the option on fld_name is for inlined anonymous structure.
    *)
    fld_name: name option;
    fld_type: type_;
  }
 (* with tarzan *)

type enum_def = { 
  e_name: fullname;
  (* we also need to use 'fullname' for constants, to scope them *)
  e_constants: (fullname * const_expr option) list;
}
 (* with tarzan *)

(* to manage the scope of tags *)
type tagkind =
  | TagStruct
  | TagUnion
  | TagEnum

type type_def = { 
  t_name: fullname;
  t_type: type_;
}
 (* with tarzan *)


(* ------------------------------------------------------------------------- *)
(* Program *)
(* ------------------------------------------------------------------------- *)
type toplevel =
  | StructDef of struct_def
  | TypeDef of type_def
  | EnumDef of enum_def

  | FuncDef of func_def
  (* contains extern decls and prototypes *)
  | VarDecl of var_decl
 (* with tarzan *)

type program = toplevel list
 (* with tarzan *)

(* ------------------------------------------------------------------------- *)
(* Any *)
(* ------------------------------------------------------------------------- *)

(* for visitor and dumper *)
type any =
  | Expr of expr
  | Stmt of stmt
  | Type of type_
  | Toplevel of toplevel
  | Program of program

 (* with tarzan *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
