(*s: mk/Globals.ml *)

(* Mostly used by lexer but also by parser, and by error management code.
 * We can not define it in the lexer because of a mutual dependency issue.
 * We can not define it in the parser because it's not exported in the .mli.
 *)

(*s: constant [[Globals.line]] *)
let line = ref 1
(*e: constant [[Globals.line]] *)
(*s: constant [[Globals.file]] *)
let file = ref "<nofile>"
(*e: constant [[Globals.file]] *)
(*e: mk/Globals.ml *)
