(* Need:
 * - open_in: for argv derived file but also for #include'd files
 *   because 5c does its own preprocessing
 * - open_out for -o object file or 5.argv[0]
 * - env: for INCLUDE (for cpp)
 *)
type caps = < Cap.open_in; Cap.open_out; Cap.env >

(* entry point (can also raise Exit.ExitCode) *)
val main: <caps; ..> -> string array -> Exit.t

(* main algorithm *)
(* val compile: ... *)
