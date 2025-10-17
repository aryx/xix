(* Need:
 * - open_in but should be only for argv derived file
 * - open_out for -o object file or 5.argv[0]
 *)
type caps = < Cap.open_in; Cap.open_out >

(* entry point (can also raise Exit.ExitCode) *)
val main: <caps; ..> -> string array -> Exit.t

(* main algorithm; works by side effect on outfile *)
val link5: 
  < Cap.open_in; ..> ->
  Types.config -> Fpath.t list (* objfiles *) -> Chan.o (* outfile *) -> unit
