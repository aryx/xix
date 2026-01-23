
type caps = < Cap.stdout; Cap.stdin; Cap.open_in >

val main: <caps; Cap.stderr; ..> ->
  string array -> Exit.t

type conf = {
  iflag : bool;
  nflag: bool;
}

(* internal, and core algorithm *)
val grep: < Cap.stdout; ..> -> conf -> string (* regexp *) -> Chan.i -> unit
