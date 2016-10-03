(* Copyright 2016 Yoann Padioleau, see copyright.txt *)

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* An Abstract Syntax Tree (AST) for cpp directives.
*)

(*****************************************************************************)
(* The AST *)
(*****************************************************************************)

type directive =
  | Include of Common.filename * bool (* true if <>, false if "" *)

  | Define of macro
  | Undef of string

  | Ifdef of string
  | Ifndef of string
  | Else
  | Endif

  | Line of int * Common.filename
  (* ex: #pragma lib "libc.a" -> Pragma("lib", ["libc.a"]) *)
  | Pragma of string * string list

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
