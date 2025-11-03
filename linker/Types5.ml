(*s: Types5.ml *)
(* Copyright 2016, 2025 Yoann Padioleau, see copyright.txt *)
open Common

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(*s: function [[Types5.show_instr]] *)
(* for ocaml-light, to work without deriving *)
let show_instr _ = "NO DERIVING"
[@@warning "-32"]
(*e: function [[Types5.show_instr]] *)

(*s: type [[Types5.instr]] *)
type instr = Ast_asm5.instr_with_cond Types.code_bis
[@@deriving show]
(*e: type [[Types5.instr]] *)

(*s: type [[Types5.node]] *)
type node = Ast_asm5.instr_with_cond Types.node
[@@deriving show]
(*e: type [[Types5.node]] *)

(*s: type [[Types5.code_graph]] *)
type code_graph = Ast_asm5.instr_with_cond Types.code_graph
[@@deriving show]
(*e: type [[Types5.code_graph]] *)
(*e: Types5.ml *)
