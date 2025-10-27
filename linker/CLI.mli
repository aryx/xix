(* Need:
 * - open_in but should be only for argv derived files
 * - open_out for -o exec file or 5.out
 *)
type caps = < Cap.open_in; Cap.open_out >

(* entry point (can also raise Exit.ExitCode) *)
val main: <caps; ..> ->
  string array -> Exit.t

(* main algorithm; works by side effect on outfile *)
val link: < Cap.open_in; ..> ->
  Arch.t -> Types.config -> Fpath.t list (* files *) -> Chan.o (* outfile *) ->
  unit
