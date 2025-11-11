(* Copyright 2025 Yoann Padioleau, see copyright.txt *)
open Common

module A = Ast_asm
module T = Types

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let rewrite (conf : Exec_file.profile_kind) (_syms : T.symbol_table) (cg : 'i T.code_graph) : 'i T.code_graph * T.data list =
  Logs.info (fun m -> m "Adding profiling instrumentation %s" 
        (Exec_file.show_profile_kind conf));
  cg, []
