
(* Mostly used by lexer but also by parser, and by error managment code.
 * We can not define it in the lexer because of mutual dependency issue.
 * We can not define it in the parser because it's not exported in .mli.
 *)
let line = ref 1
