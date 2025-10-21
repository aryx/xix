(* Copyright 2016, 2025 Yoann Padioleau, see copyright.txt *)
open Common

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* for ocaml-light, to work without deriving *)
let show_instr _ = "NO DERIVING"
[@@warning "-32"]

type instr = Ast_asm5.instr_with_cond Types.code_bis
[@@deriving show]

type node = Ast_asm5.instr_with_cond Types.node
[@@deriving show]

type code_graph = Ast_asm5.instr_with_cond Types.code_graph
[@@deriving show]
