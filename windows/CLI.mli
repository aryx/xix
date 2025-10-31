(* Need:
 * - draw/mouse/keyboard because rio multiplexes access to those devices
 *)
type caps = < Cap.draw; Cap.mouse; Cap.keyboard >

(* entry point (can also raise Exit.ExitCode) *)
val main: <caps; ..> -> string array -> Exit.t

(* main thread which will itself create mouse/keyboard/fs/... threads *)
val thread_main: < caps; ..> -> Exit.t
