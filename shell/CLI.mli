(*s: CLI.mli *)

(*s: type [[CLI.caps]] *)
(* Need:
 *  - fork/exec: obviously as we are a shell
 *  - chdir: for the builtin 'cd'
 *  - env: to ??
 *  - exit: as many commands can abruptely exit 'rc' itself or children
 *    created by 'rc'
 *  - open_in: ??
 *
 * alt: could remove Cap.exit and use Exit.ExitCode exn in Process.ml instead
*)
type caps = < Cap.fork; Cap.exec; Cap.chdir; Cap.env; Cap.exit; Cap.open_in >
(*e: type [[CLI.caps]] *)

(*s: signature [[CLI.main]] *)
(* entry point (can also raise Exit.ExitCode) *)
val main: <caps; ..> -> string array -> Exit.t
(*e: signature [[CLI.main]] *)

(* internals *)
(*s: signature [[CLI.interpret_bootstrap]] *)
val interpret_bootstrap : < caps > -> string list -> unit
(*e: signature [[CLI.interpret_bootstrap]] *)
(*e: CLI.mli *)
