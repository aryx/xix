(* Need:
 * - open_in for ?
 *)
type caps = < Cap.open_in >

(* entry point (can also raise Exit.ExitCode) *)
val main: <caps; ..> -> string array -> Exit.t

(* main algorithm ? *)
