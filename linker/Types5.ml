(* Copyright 2016 Yoann Padioleau, see copyright.txt *)
open Common

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type code = Ast_asm5.instr_with_cond Types.code
type node = Ast_asm5.instr_with_cond Types.node
type code_graph = node (* the first node *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
