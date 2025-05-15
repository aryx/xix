
(* Need:
 *  - fork/exec: obviously as we run shell commands
 *  - env: for Env.initenv() so mk recipe can access env variables.
 *    Also MKSHELL in Shell.ml and NPROC in Scheduler.ml
 *  - argv: for setting MKFLAGS also in Env.initenv()
 *  - chdir: actually needed just for -debugger, we could remove
 *)
type caps = < Cap.fork; Cap.exec; Cap.env; Cap.argv; Cap.chdir >

(* entry point (can also raise Exit.ExitCode) *)
val main: <caps; Cap.stdout; ..> -> string array -> Exit.t

(* main algorithm *)
val build_target :
  caps -> Env.t -> Rules.rules -> string (* target *) -> unit                                                       
val build_targets :
  caps -> Fpath.t -> string list ref -> (string*string) list -> unit
