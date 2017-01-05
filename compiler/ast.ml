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

(* global linenumber after preprocessing *)
type loc = Location_cpp.loc

(* ------------------------------------------------------------------------- *)
(* Name *)
(* ------------------------------------------------------------------------- *)

type name = string

(* for scope *)
type blockid = int (* same than Type.blockid, repeated here for clarity *)

(* A fully resolved and scoped name. 
 * 5c uses a reference to a symbol in a symbol table to fully qualify a name.
 * Instead, I use a unique blockid and an external hash or environment that
 * maps this fullname to the appropriate information.
 * I think it offers a better separation of concerns.
 * 
 * 'name' below can be a gensym'ed name for anonymous struct/union/enum.
 *)
type fullname = name * blockid (* same than Type.fullname *)

(* Used in globals.ml/lexer.mll/parser.mly to recognize typedef identifiers.
 * Could be moved in a separate naming.ml, but not worth it for just two types.
 *)
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
 * 
 * Note that 'type_' and 'expr' are mutually recursive (because of const_expr).
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

  | TStructName of Type.struct_kind * fullname
  (* In C an enum is really like an int. However, we could do
   * extended checks at some point to do more strict type checking! 
   *)
  | TEnumName of fullname
  | TTypeName of fullname

 and function_type = (type_ * (parameter list * bool (* var args '...' *)))

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

    p_type: type_;
  }


(* ------------------------------------------------------------------------- *)
(* Expression *)
(* ------------------------------------------------------------------------- *)
and expr = { 
  e: expr_bis;
  e_loc: loc;
  (* properly set during typechecking in typecheck.ml *)
  e_type: Type.t;
}
  and expr_bis = 
  (* Note that characters are transformed in Int at parsing time; no need Char*)
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
  | Cast of type_ * expr

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
  | SizeOf of (expr, type_) Common.either

  (* should appear only in a variable initializer, or after GccConstructor *)
  | ArrayInit of (const_expr option * expr) list
  | RecordInit of (name * expr) list
  (* gccext: kenccext: *)
  | GccConstructor  of type_ * expr (* always an ArrayInit (or RecordInit?) *)

and argument = expr

(* Because we call the preprocessor first, the remaining cases
 * where const_expr is not a constant are basic arithmetic expressions
 * like 2 < < 3, or enum constants.
 *)
and const_expr = expr

  and unaryOp  = 
    (* less: could be lifted up; those are really important operators *)
    | GetRef | DeRef 
    (* codegen: converted to binary operation with 0 (-x => 0-x) *)
    | UnPlus |  UnMinus 
    (* codegen: converted to -1 ^ x *)
    | Tilde 
    | Not 
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
  s: stmt_bis;
  s_loc: loc;
}
  and stmt_bis = 
  | ExprSt of expr
  (* empty statement is simply Block [] *)
  | Block of stmt list

  | If of expr * stmt * stmt

  (* expr must have an integer type; it can not be a pointer like in a If *)
  | Switch of expr * case_list

  | While of expr * stmt
  | DoWhile of stmt * expr
  | For of (expr option, var_decl list) Common.either * 
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
  v_storage: Storage.t option;
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
  (* functions have a global scope; no need for fullname here *)
  f_name: name;
  f_loc: loc;
  (* everything except Param or Auto *)
  f_storage: Storage.t option;
  f_type: function_type;
  (* always a Block *)
  f_body: stmt;
}
 (* with tarzan *)

(* struct and union *)
type struct_def = {
  su_name: fullname;
  su_loc: loc;
  su_kind: Type.struct_kind;
  (* todo: bitfield annotation *)
  su_flds: field_def list;
}
  (* Not the same than var_decl; fields have no storage and can have bitflds.*)
  and field_def = { 
   (* kenccext: anonymous structure element get an artificial field name
    * (see is_gensymed()) 
    *)
    fld_name: name;
    fld_loc: loc;
    fld_type: type_;
  }
 (* with tarzan *)

type enum_def = { 
  (* this name is rarely used; C programmers rarely write 'enum Foo x;' *)
  enum_name: fullname;
  enum_loc: loc;
  enum_constants: enum_constant list;
}
  and enum_constant = {
  (* we also need to use 'fullname' for constants, to scope them *)
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
  (* globals, but also extern decls and prototypes *)
  | VarDecl of var_decl
  | FuncDef of func_def
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
  | FinalType of Type.t

 (* with tarzan *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
let tagkind_of_su = function
  | Type.Struct -> TagStruct
  | Type.Union -> TagUnion

let unwrap (name, _) = name    

open Common
(* see also Parser.gensym *)
let is_gensymed str = 
  str =~ "|sym[0-9]+|.*"
