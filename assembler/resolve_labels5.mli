
(* The final program has no more labels (defs and uses) or 
 * relative jumps or labels in branching instructions.
 * Those are converted in absolute jumps.
 * !!Actually works by side effect on input program so take care!!
 *)
val resolve: Ast_asm5.program -> Ast_asm5.program
