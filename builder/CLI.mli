(*s: CLI.mli *)

(*s: type [[CLI.caps]] *)
(* Need:
 *  - fork/exec/wait: obviously as we run shell commands
 *  - env: for Env.initenv() so mk recipe can access env variables.
 *    Also MKSHELL in Shell.ml and NPROC in Scheduler.ml
 *  - argv: for setting MKFLAGS also in Env.initenv()
 *  - open_in: for parsing the mkfile (and included files)
 *)
type caps = < Cap.forkew; Cap.env; Cap.argv; Cap.open_in >
(*e: type [[CLI.caps]] *)

(*s: signature [[CLI.main]] *)
(* entry point (can also raise Exit.ExitCode) *)
val main: <caps; Cap.stdout; ..> ->
  string array -> Exit.t
(*e: signature [[CLI.main]] *)

(*s: signature [[CLI.build_target]] *)
(* main algorithm *)
val build_target : <caps; ..> ->
  Env.t -> Rules.rules -> string (* target *) -> unit                                                       
(*e: signature [[CLI.build_target]] *)
(*s: signature [[CLI.build_targets]] *)
val build_targets : <caps; ..> ->
  Fpath.t -> string list ref -> (string*string) list -> unit
(*e: signature [[CLI.build_targets]] *)
(*e: CLI.mli *)
