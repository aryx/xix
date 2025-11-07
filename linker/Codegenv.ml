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

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

let size_of_instruction  (_env : Codegen.env) (_node : Tv.node) : int (* a multiple of 4 *) * Codegen.pool option =
  failwith "TODO4"


let gen (_symbols2 : T.symbol_table2) (_config : Exec_file.linker_config) (_cg : Tv.code_graph) : T.word list =
  failwith "TODO5"



