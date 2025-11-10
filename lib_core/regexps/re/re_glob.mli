(*
   RE - A regular expression library
   Copyright (C) 2001 Jerome Vouillon
*)

exception Parse_error

val glob : string -> Re.t

val glob' : bool -> string -> Re.t
   (* Same, but allows to choose whether dots at the beginning of a
      file name need to be explicitly matched (true) or not (false) *)

val globx : string -> Re.t
val globx' : bool -> string -> Re.t
    (* These two functions also recognize the pattern {..,..} *)
