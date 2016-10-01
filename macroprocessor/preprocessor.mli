
type cmdline_defs = (string * string) list

type system_paths = Common.filename list

type include_paths = Common.filename * system_paths


type macro = {
  name: string;
  nbargs: int option;
  varargs: bool; (* use "..." *)
  body: string;
}

val hmacros: (string, macro) Hashtbl.t

val define_cmdline_def: 
  (string * string) -> unit

(* may raise an exception if macro was already defined *)
val define:
  (string * (string list * bool (* ... *)) option * string option) -> unit

(* may raise an exception if the file could not be found *)
val find_include:
  include_paths -> (string * bool (* system header (<>) *)) -> Common.filename
