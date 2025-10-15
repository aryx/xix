(* Need:
 * - open_in: for argv derived file but also for #include'd files
 *   because 5a is a macroassembler
 * - env: for INCLUDE (for cpp)
 * - TODO open_out for -o object file or argv .5
 *)
type caps = < Cap.open_in; Cap.env >

(* entry point (can also raise Exit.ExitCode) *)
val main: <caps; ..> -> string array -> Exit.t

(* main algorithm *)
(* val assemble5: ... *)
