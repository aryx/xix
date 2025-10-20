
(* The final program has neither labels (defs and uses) nor 
 * relative jumps in branching instructions.
 * Those are converted in absolute jumps.
 * !!Actually works by side effect on input program so take care!!
 *)
val resolve: 
  ('instr -> Ast_asm.branch_operand option) ->
  'instr Ast_asm.program -> 'instr Ast_asm.program
