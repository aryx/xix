
(* The final program has neither labels (defs and uses) nor 
 * relative jumps in branching instructions.
 * Those are converted in absolute jumps.
 * !!Actually works by side effect on input program so take care!!
 *)
val resolve: Ast_asm5.program -> Ast_asm5.program
