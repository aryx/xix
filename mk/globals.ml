
(* Mostly used by lexer but also by parser, and by error managment code.
 * We can not define it in the lexer because of a mutual dependency issue.
 * We can not define it in the parser because it's not exported in the .mli.
 *)
let line = ref 1

let file = ref "<nofile>"

