(* capabilities-aware version of UConsole.ml *)

let print (_caps : < Cap.stdout; .. > ) = UConsole.print
let print_no_nl (_caps : < Cap.stdout; .. >) = UConsole.print_no_nl
let eprint (_caps : < Cap.stderr; .. >) = UConsole.eprint
