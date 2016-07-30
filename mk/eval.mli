
(* Evaluate the variables and backquotes, process included files,
 * index the rules, and return the final environment (actually modify
 * by side effect the environment).
 * 
 * Also sets the parameter to contain the first (simple) target found
 * in the file if it was not set already.
 * 
 * Will call parse() and eval() recursively to process <file instructions.
 *)
val eval: 
  Env.t -> (string list ref) -> Ast.instr list -> 
  Rules.t * Env.t
