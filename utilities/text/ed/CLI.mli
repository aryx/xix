(*s: CLI.mli *)
(*s: type [[CLI.caps]] *)
type caps = < 
    Cap.stdin; Cap.stdout; Cap.stderr;
    Cap.open_in; Cap.open_out;
  >
(*e: type [[CLI.caps]] *)
(*s: signature [[CLI.main]] *)
val main: <caps; ..> ->
  string array -> Exit.t
(*e: signature [[CLI.main]] *)
(*e: CLI.mli *)
