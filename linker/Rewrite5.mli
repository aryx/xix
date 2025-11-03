(*s: Rewrite5.mli *)

(*s: signature [[Rewrite5.rewrite]] *)
(* Mostly TEXT/RET rewrite depending whether a function is a "leaf".
 * !!actually works by side effect on graph so take care!! 
 * may raise Failure in case of error.
*)
val rewrite: 
  Types5.code_graph -> Types5.code_graph
(*e: signature [[Rewrite5.rewrite]] *)
(*e: Rewrite5.mli *)
