(*s: macroprocessor/Ast_cpp.ml *)
(* Copyright 2016, 2025 Yoann Padioleau, see copyright.txt *)

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* An Abstract Syntax Tree (AST) for the C Pre-Processor (CPP) directives.
*)

(*****************************************************************************)
(* The AST *)
(*****************************************************************************)

(*s: type [[Ast_cpp.directive]] *)
type directive =
  | Include of Fpath.t * bool (* true if <>, false if "" *)

  | Define of macro
  | Undef of string

  | Ifdef of string
  | Ifndef of string
  | Else
  | Endif

  | Line of int * Fpath.t
  (* ex: #pragma lib "libc.a" -> Pragma("lib", ["libc.a"]) *)
  | Pragma of string * string list
(*e: type [[Ast_cpp.directive]] *)

(*s: type [[Ast_cpp.macro]] *)
  and macro = {
    name: string;
    (* Note that you can have None or Some [] for parameters.
     * The first is a macro without parameter, the second a macro with 
     * 0 parameters (but that still needs to be called by FOO())
     *)
    params: string list option;
    varargs: bool;
    (* The body below has been processed and every parameter occurence
     * is replaced by a #xxx where xxx is the number corresponding to the
     * ith parameter.
     *)
    body: string option
  }
(*e: type [[Ast_cpp.macro]] *)
[@@deriving show]
(*e: macroprocessor/Ast_cpp.ml *)
