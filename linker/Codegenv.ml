(* Copyright 2025 Yoann Padioleau, see copyright.txt *)
open Common
open Either

open Ast_asm
open Ast_asmv

module T = Types
module Tv = Typesv
open Types
open Typesv

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Mips code generation.
 *
 *)

(*****************************************************************************)
(* Types and constants *)
(*****************************************************************************)

type pool =
  (* note that it is not always an int! Sometimes it can be an
   * Address which will be resolved only at the very end.
   *)
  | PoolOperand of Ast_asm.ximm
  (* todo: still don't know why we need that *)
  | LPOOL 

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

let size_of_instruction (_symbols2 : T.symbol_table2) (_autosize : int) (_node : Tv.node) : int (* a multiple of 4 *) * pool option =
  failwith "TODO4"


let gen (_symbols2 : T.symbol_table2) (_config : Exec_file.linker_config) (_cg : Tv.code_graph) : T.word list =
  failwith "TODO5"



