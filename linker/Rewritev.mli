
(* Mostly TEXT/RET rewrite depending whether a function is a "leaf".
 * !!actually works by side effect on graph so take care!! 
 * may raise Failure in case of error.
*)
val rewrite: 
  Ast_asmv.instr Types.code_graph -> Ast_asmv.instr Types.code_graph
