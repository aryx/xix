(* Copyright 2016, 2025 Yoann Padioleau, see copyright.txt *)
open Common

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* for ocaml-light, to work without deriving *)
let show_instr _ = "NO DERIVING"
[@@warning "-32"]

type instr = Ast_asmv.instr Types.code_bis
[@@deriving show]

type node = Ast_asmv.instr Types.node
[@@deriving show]

type code_graph = Ast_asmv.instr Types.code_graph
[@@deriving show]
