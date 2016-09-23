(* Copyright 2016 Yoann Padioleau, see copyright.txt *)

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* An Abstract Syntax Tree (AST) for C.
 * 
 * See also pfff/lang_c/parsing/ast_c.ml and pfff/lang_cpp/parsing/ast_cpp.ml
 *)

(*****************************************************************************)
(* The AST related types *)
(*****************************************************************************)

(* global linenumber *)
type loc = int 

(* ------------------------------------------------------------------------- *)
(* Name *)
(* ------------------------------------------------------------------------- *)

(* less: ref to symbol? or use external hash? 
 * less: set later a blockid so unambiguous?
 * todo: lineno field?
 *)
type name = string
(* ------------------------------------------------------------------------- *)
(* Types *)
(* ------------------------------------------------------------------------- *)

(* ------------------------------------------------------------------------- *)
(* Expression *)
(* ------------------------------------------------------------------------- *)
(* todo: lineno field *)
and expr =
  | ExprTodo

(* ------------------------------------------------------------------------- *)
(* Statement *)
(* ------------------------------------------------------------------------- *)
(* todo: lineno field *)
and stmt =
  (* have a specific case type? hard in C because they mix labels
   * and case a lot (see the lexer of 5c).
   *)
  | Switch

  | StmtTodo

(* ------------------------------------------------------------------------- *)
(* Variables *)
(* ------------------------------------------------------------------------- *)
and var =
  | VarTodo

(* ------------------------------------------------------------------------- *)
(* Definitions *)
(* ------------------------------------------------------------------------- *)
and def = 
  | DefTodo

(* ------------------------------------------------------------------------- *)
(* Program *)
(* ------------------------------------------------------------------------- *)
type program = unit

(* ------------------------------------------------------------------------- *)
(* Any *)
(* ------------------------------------------------------------------------- *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
