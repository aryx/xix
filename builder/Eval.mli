(*s: Eval.mli *)
(*s: signature [[Eval.eval]] *)
(* Evaluate the variables and backquotes in the mkfile (hence Cap.exec),
 * process the included files (hence Cap.open_in), index the rules, and return
 * the final environment (actually modify by side effect the environment).
 * 
 * Also sets the parameter to contain the first (simple) target found
 * in the file if it was not set already.
 * 
 * Note that eval() will call parse() internally as well as eval() itself
 * recursively to process '<file' instructions.
 *)
val eval :
  < Shell.caps ; Cap.open_in; .. > ->
  Env.t ->
  string list ref ->
  Ast.instr list ->
  Rules.rules * Env.t
(*e: signature [[Eval.eval]] *)

(* internals *)
(*s: signature [[Eval.eval_words]] *)
val eval_words : < Shell.caps; .. > -> Ast.loc -> Env.t ->
  Ast.words -> (string list, Percent.pattern list) Either.t
(*e: signature [[Eval.eval_words]] *)
(*e: Eval.mli *)
