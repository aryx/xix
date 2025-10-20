
(* !! will modify the code to resolve SymbolJump so take care!! 
 * raise Failure in case of error.
*)
val build_graph:
  ('instr -> Ast_asm.branch_operand option) ->
  Types.symbol_table -> 'instr Types.code array -> 'instr Types.code_graph
