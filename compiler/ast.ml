(* Copyright 2016 Yoann Padioleau, see copyright.txt *)

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
 *  - no mix of typedefs with variable declarations;
 *    again they are lifted to the top
 *  - enums are also lifted to the top (and its constants are tagged with
 *    a blockid)
 * 
 * This AST is actually more a named AST (but not a typed AST). 
 * Indeed, in C the naming phase can not be a separate phase after parsing. 
 * The grammar of C has an ambiguity with typedefs, so we need to keep track of 
 * typedefs and identifiers and their scope during parsing. It would be
 * redundant to do this work again in a separate naming phase, so I 
 * name and resolve the scope of identifiers at parsing time.
 * Moreover, because I lift up struct definitions, I also need to keep track
 * and resolve the scope of tags.
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

type name = string

(* for scope *)
type blockid = int

(* A fully resolved and scoped name. 
 * 5c uses a reference to a symbol in a symbol table. Instead, I use external
 * hash or environment each time. I think it offers a better separation 
 * of concerns.
 * 'name' below can be a gensym'ed name for anonymous struct/union/enum.
 *)
type fullname = name * blockid

(* Used in globals.ml/lexer.mll/parser.mly to recognize typedef identifiers. *) 
type idkind =
  | IdIdent
  | IdTypedef
  | IdEnumConstant

(* to manage the scope of tags *)
type tagkind =
  | TagStruct
  | TagUnion
  | TagEnum

(* ------------------------------------------------------------------------- *)
(* Types *)
(* ------------------------------------------------------------------------- *)
(* What are the differences between type_ below and Type.t? 
 * - typedef expansion is not done here
 * - constant expressions are not resolved yet 
 *  (those expressions can involve enum constants which will be resolved later).
 * Again, I think it offers a better separation of concerns.
 * todo: qualifier type
 *)
type type_ = {
  t: type_bis;
  t_loc: loc;
}
  and type_bis = 
  | TBase of Type.t (* only the Basic stuff *)
  | TPointer of type_
  | TArray of const_expr option * type_
  | TFunction of function_type

  | TStructName of struct_kind * fullname
  (* In C an enum is really like an int. However, I could do
   * extended checks at some point to do more strict type checking! 
   *)
  | TEnumName of fullname
  | TTypeName of fullname

 and function_type = (type_ * (parameter list * bool (* var args '...' *)))

  and parameter = {
    (* When part of a prototype, the name is not always mentionned, hence
     * the option below.
     * I use 'fullname' here for consistency; parameters are treated like,
     * locals, so we can have below simply 'Id of fullname' and have 
     * no differences between accessing a local or a parameter.
     *)
    p_name: fullname option;
    p_loc: loc;

    p_type: type_;
  }

 and struct_kind = Struct | Union

(* ------------------------------------------------------------------------- *)
(* Expression *)
(* ------------------------------------------------------------------------- *)
(* todo: mutable e_type: Type.t? *)
and expr = { 
  e: expr_bis;
  e_loc: loc;
}
  and expr_bis = 
  (* Note that characters are transformed in Int at parsing time; no need Char*)
  | Int of string * Type.sign * Storage.intsize
  | Float of string * Storage.floatsize
  | String of string * Storage.stringsize

  (* Global, local, parameter, enum constant (can be scoped), function *)
  (* todo: mutable symkind? storage? type? setused? *)
  | Id of fullname

  | Call of expr * argument list

  (* should be a statement really *)
  | Assign of assignOp * expr * expr

  | ArrayAccess of expr * expr (* x[y] *)
  | RecordAccess of expr * name (* x.y *)
  (* less? keep just x->y? Why x->y instead of x.y choice?
   * it's more consistent with ArrayAccess where expr has to be
   * a kind of pointer too. That means x.y is actually unsugared in (&x)->y
   *)
  | RecordPtAccess of expr * name (* x->y,  and not x.y!! *)

  (* less: bool (* explicit cast (xcast) *) *)
  | Cast of type_ * expr

  | Postfix of expr * fixOp
  | Prefix of fixOp * expr
  (* contains GetRef and Deref!!  *)
  | Unary of unaryOp * expr
  | Binary of expr * binaryOp * expr

  | CondExpr of expr * expr * expr
  (* 'x, y', but really should be a statement *)
  | Sequence of expr * expr

  | SizeOf of (expr, type_) Common.either

  (* should appear only in a variable initializer, or after GccConstructor *)
  | ArrayInit of (const_expr option * expr) list
  | RecordInit of (name * expr) list
  (* gccext: kenccext: *)
  | GccConstructor  of type_ * expr (* always an ArrayInit (or RecordInit?) *)

and argument = expr

(* Now that we call the preprocessor first, the remaining cases
 * where const_expr is not a constant are basic arithmetic expressions
 * like 2 < < 3, or enum constants.
 *)
and const_expr = expr

  and unaryOp  = 
    (* less: could be lift up; those are really important operators *)
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
type stmt = {
  st: stmt_bis;
  stmt_loc: loc;
}
  and stmt_bis = 
  | ExprSt of expr
  | Block of stmt list

  | If of expr * stmt * stmt

  | Switch of expr * case_list

  | While of expr * stmt
  | DoWhile of stmt * expr
  | For of (expr option, var_decl list) Common.either * 
           expr option * 
           expr option * 
           stmt

  | Return of expr option
  | Continue | Break

  (* labels have a function scope, so no need to use 'fullname' here *)
  | Label of name * stmt
  | Goto of name

  (* should occur only in Switch *)
  | Case of expr * stmt
  | Default of stmt

  | Var of var_decl

(* Can we have a specific case type? It is hard in C because they mix labels
 * and 'case' a lot (see the code in the lexer of 5c).
 *)
and case_list = stmt

(* ------------------------------------------------------------------------- *)
(* Variables *)
(* ------------------------------------------------------------------------- *)

and var_decl = {
  v_name: fullname;
  v_loc: loc;
  v_storage: Storage.t; (* less: Storage option? *)
  v_type: type_;
  v_init: initialiser option;
}
 (* can have ArrayInit and RecordInit here in addition to other expr *)
 and initialiser = expr
 (* with tarzan *)


(* ------------------------------------------------------------------------- *)
(* Definitions *)
(* ------------------------------------------------------------------------- *)
type func_def = {
  (* functions have a global scope, no need for fullname here *)
  f_name: name;
  f_loc: loc;
  (* everything except Param or Auto *)
  f_storage: Storage.t;
  f_type: function_type;
  (* always a Block *)
  f_body: stmt;
}
 (* with tarzan *)

type struct_def = {
  s_name: fullname;
  s_loc: loc;
  s_kind: struct_kind;
  s_flds: field_def list;
}
  (* We could merge with var_decl, but fields have no storage, and
   * they can have bitfields.
   *)
  and field_def = { 
   (* todo: bitfield annotation
    * kenccext: the option on fld_name is for inlined anonymous structure.
    *)
    fld_name: name option;
    fld_loc: loc;
    fld_type: type_;
  }
 (* with tarzan *)

type enum_def = { 
  e_name: fullname;
  e_loc: loc;
  e_constants: enum_constant list;
}
  and enum_constant = {
  (* we also need to use 'fullname' for constants, to scope them.
   * less: actually seems like enum constants have a global scope?
   *)
    ecst_name: fullname;
    ecst_loc: loc;
    ecst_value: const_expr option;
  }
 (* with tarzan *)

type type_def = { 
  typedef_name: fullname;
  typedef_loc: loc;
  typedef_type: type_;
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
