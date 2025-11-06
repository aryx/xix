
(* Mostly TEXT/RET rewrite depending whether a function is a "leaf".
 * !!actually works by side effect on graph so take care!! 
 * may raise Failure in case of error.
*)
val rewrite: 
  Typesv.code_graph -> Typesv.code_graph
