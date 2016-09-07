
(* -i (or by default when detects that stdin is /dev/cons *)
let interactive = ref true
(* -l (or by default if argv0 starts with a -) *)
let login = ref false


let dump_tokens = ref false
let dump_ast = ref false

let debugger = ref false
