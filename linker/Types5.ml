(* Copyright 2016, 2025 Yoann Padioleau, see copyright.txt *)
open Common

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type node = Ast_asm5.instr_with_cond Types.node
type code_graph = Ast_asm5.instr_with_cond Types.code_graph
