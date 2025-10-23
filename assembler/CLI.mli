(* Need:
 * - open_in: for argv derived input file but also for #include'd files
 *   because 5a is a macroassembler
 * - open_out for -o object file or argv[0].5
 * - env: for INCLUDE (for cpp)
 *)
type caps = < Cap.open_in; Cap.open_out; Cap.env >

(* entry point (can also raise Exit.ExitCode) *)
val main: <caps; ..> ->
  string array -> Exit.t

(* main algorithm; works by side effect on outfile *)
val assemble: <Cap.open_in; .. > ->
  Preprocessor.conf -> Arch.t -> Fpath.t (* infile *) -> Chan.o (* outfile *) ->
  unit
