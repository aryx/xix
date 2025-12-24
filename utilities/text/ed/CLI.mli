
type caps = < 
    Cap.stdin; Cap.stdout; Cap.stderr;
    Cap.open_in; Cap.open_out;
  >

val main: <caps; ..> ->
  string array -> Exit.t
