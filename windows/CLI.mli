(*s: CLI.mli *)
(*s: type [[CLI.caps]] *)
(* Need:
 * - draw/mouse/keyboard because rio multiplexes access to those devices
 * - fork/exec/chdir when creating new windows which trigger new rc
 *   processes run possibly from different directories.
 * - open_in: for /dev/winname access
 * - mount/bind: for the window to mount the rio fileserver to /mnt/wsys
 *   and then bind it to /dev for virtual /dev/{cons,mouse,...}
 *)
type caps = < 
    Cap.draw; Cap.mouse; Cap.keyboard;
    Cap.fork; Cap.exec; Cap.chdir;
    Cap.open_in;
    Cap.mount; Cap.bind
  >
(*e: type [[CLI.caps]] *)

(*s: signature [[CLI.main]] *)
(* entry point (can also raise Exit.ExitCode) *)
val main: <caps; Cap.stdout; Cap.stderr; ..> ->
  string array -> Exit.t
(*e: signature [[CLI.main]] *)
(*s: signature [[CLI.thread_main]] *)
(* main thread which will itself create mouse/keyboard/fs/... threads *)
val thread_main: < caps; ..> -> Exit.t
(*e: signature [[CLI.thread_main]] *)
(*e: CLI.mli *)
