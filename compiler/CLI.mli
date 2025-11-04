(*s: CLI.mli *)
(* Need:
 * - open_in: for argv derived file but also for #include'd files
 *   because 5c does its own preprocessing
 * - open_out for -o object file or 5.argv[0]
 * - env: for INCLUDE (for cpp)
 *)
(*s: type [[CLI.caps (CLI.mli)]] *)
type caps = < Cap.open_in; Cap.open_out; Cap.env >
(*e: type [[CLI.caps (CLI.mli)]] *)

(*s: signature [[CLI.main]] *)
(* entry point (can also raise Exit.ExitCode) *)
val main: <caps; ..> ->
  string array -> Exit.t
(*e: signature [[CLI.main]] *)

(*s: signature [[CLI.compile]] *)
(* main algorithm; works by side effect on outfile *)
val compile: < Cap.open_in; .. > ->
  Preprocessor.conf -> Arch.t -> Fpath.t (* infile *) -> Chan.o (* outfile *) ->
  unit
(*e: signature [[CLI.compile]] *)
(*e: CLI.mli *)
