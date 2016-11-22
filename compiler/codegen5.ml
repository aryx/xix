(* Copyright 2016 Yoann Padioleau, see copyright.txt *)
open Common

module C = Ast
module Asm = Ast_asm5

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* gclean:
 * - generate all AGLOBL
 * - generate last opcode AEND
 *  use ast_asm_common.ml?
 *)

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let codegen ast =
  raise Todo
