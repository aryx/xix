(* Copyright 2016 Yoann Padioleau, see copyright.txt *)

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* An Abstract Syntax Tree (AST) for cpp directives.
*)

(*****************************************************************************)
(* The AST related types *)
(*****************************************************************************)

type directive =
  | Include of Common.filename * bool (* true if <>, false if "" *)

  (* the define body has been processed and every parameter occurence
   * is replaced by a #xxx where xxx is the number corresponding to the
   * ith parameter.
   * Note that you can have Define (,None,) and Define (,Some ([],false),)
   *)
  | Define of string * (string list * bool (* ... *)) option * string option
  | Undef of string

  | Ifdef of string
  | Ifndef of string
  | Else
  | Endif

  | Line of int * Common.filename

  | Pragma of string * string
