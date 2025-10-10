(* Need:
 * - env: for INCLUDE (for cpp)
 *)
type caps = < Cap.env >

(* entry point (can also raise Exit.ExitCode) *)
val main: <caps; ..> -> string array -> Exit.t

(* main algorithm *)
(* val assemble5: ... *)
