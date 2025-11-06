(* Copyright 2025 Yoann Padioleau, see copyright.txt *)
open Common

module T = Types
module Tv = Typesv
module A = Ast_asm

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

let layout_text (_symbols2 : T.symbol_table2) (_init_text : T.real_pc) (_cg : Tv.code_graph) : T.symbol_table2 * Tv.code_graph * int =
  failwith "TODO3"

