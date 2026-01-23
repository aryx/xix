type caps = < Cap.stdout; >
val main: <caps; Cap.stderr; ..> ->
  string array -> Exit.t
