(*s: CLI.mli *)

(* Need:
 *  - fork/exec: obviously as we are a shell
 *  - chdir: for the builtin 'cd'
 *  - env: to ??
 *  - exit: as many commands can abruptely exit 'rc' itself or children
 *    created by 'rc'
 *  - open_in: ??
 *)
(*s: type [[CLI.caps (CLI.mli)]] *)
type caps = < Cap.fork; Cap.exec; Cap.chdir; Cap.env; Cap.exit; Cap.open_in >
(*e: type [[CLI.caps (CLI.mli)]] *)

(*s: signature [[CLI.main]] *)
(* entry point (can also raise Exit.ExitCode) *)
val main: <caps; ..> -> string array -> Exit.t
(*e: signature [[CLI.main]] *)
(*e: CLI.mli *)
