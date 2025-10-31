(* Need:
 * - draw/mouse/keyboard because rio multiplexes access to those devices
 * - fork/exec/chdir when creating new windows which trigger new rc
 *   processes run possibly from different directories.
 *)
type caps = < Cap.draw; Cap.mouse; Cap.keyboard; Cap.fork; Cap.exec; Cap.chdir >

(* entry point (can also raise Exit.ExitCode) *)
val main: <caps; ..> -> string array -> Exit.t

(* main thread which will itself create mouse/keyboard/fs/... threads *)
val thread_main: < caps; ..> -> Exit.t
