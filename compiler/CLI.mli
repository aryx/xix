(* Need:
 * - env: for INCLUDE (for cpp)
 * - TODO open_in but should be only for argv derived file
 * - TODO open_out for -o object file or 5. argv 
 *)
type caps = < Cap.env >

(* entry point (can also raise Exit.ExitCode) *)
val main: <caps; ..> -> string array -> Exit.t

(* main algorithm *)
(* val compile: ... *)
