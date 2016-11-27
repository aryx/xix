(* Copyright 2016 Yoann Padioleau, see copyright.txt *)
open Common

module C = Ast
module Asm = Ast_asm5

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 * todo:
 *   - compute offset for local/params
 *   - firstarg opti
 *   - structure alignment (sualign)
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let width_of_type t =
  raise Todo


(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

(* gclean:
 * - generate all AGLOBL
 * - use ast_asm_common.ml?
 *)

let codegen ast =
  raise Todo
