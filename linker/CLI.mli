(*s: CLI.mli *)
(*s: type [[CLI.caps]] *)
(* Need:
 * - open_in but should be only for argv derived files
 * - open_out for -o exec file or 5.out
 *)
type caps = < Cap.open_in; Cap.open_out >
(*e: type [[CLI.caps]] *)

(*s: signature [[CLI.main]] *)
(* entry point (can also raise Exit.ExitCode) *)
val main: <caps; Cap.stdout; Cap.stderr; ..> ->
  string array -> Exit.t
(*e: signature [[CLI.main]] *)

(*s: signature [[CLI.link]] *)
(* main algorithm; works by side effect on outfile *)
val link: < Cap.open_in; ..> ->
  Arch.t -> Exec_file.linker_config -> Fpath.t list (* files *) ->
  Chan.o (* outfile *) ->
  unit
(*e: signature [[CLI.link]] *)
(*e: CLI.mli *)
