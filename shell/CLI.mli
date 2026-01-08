(*s: CLI.mli *)

(*s: type [[CLI.caps]] *)
(* Need:
 *  - fork/exec/wait: obviously as we are a shell
 *  - chdir: for the builtin 'cd'
 *  - env: to ??
 *  - exit: as many commands can abruptely exit 'rc' itself or children
 *    created by 'rc'
 *  - open_in: for '.' that can source and eval scripts
 *
 * alt: could remove Cap.exit and use Exit.ExitCode exn in Process.ml instead
*)
type caps = < Cap.forkew; Cap.chdir; Cap.env; Cap.exit; Cap.open_in >
(*e: type [[CLI.caps]] *)

(*s: signature [[CLI.main]] *)
(* entry point (can also raise Exit.ExitCode) *)
val main: <caps; Cap.stdout; Cap.stderr; ..> ->
  string array -> Exit.t
(*e: signature [[CLI.main]] *)

(* internals *)
(*s: signature [[CLI.interpret_bootstrap]] *)
val interpret_bootstrap : < caps > ->
  string list -> unit
(*e: signature [[CLI.interpret_bootstrap]] *)
(*e: CLI.mli *)
