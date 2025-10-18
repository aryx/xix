
(* !! will modify the code to resolve SymbolJump so take care!! 
 * raise Failure in case of error.
*)
val build_graph:
  Types.symbol_table -> Types5.code array -> Types5.code_graph
