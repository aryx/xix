(*s: CLI.mli *)
(*s: type [[CLI.caps]] *)
type caps = < 
    Cap.stdin; Cap.stdout; Cap.stderr;
    Cap.open_in; (* for 'r' *)
    Cap.open_out; (* for 'w' *)
    Cap.forkew; (* for '!' *)
  >
(*e: type [[CLI.caps]] *)
(*s: signature [[CLI.main]] *)
val main: <caps; ..> ->
  string array -> Exit.t
(*e: signature [[CLI.main]] *)
(*e: CLI.mli *)
