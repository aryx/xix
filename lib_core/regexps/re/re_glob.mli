(*
   RE - A regular expression library
   Copyright (C) 2001 Jerome Vouillon
*)

exception Parse_error

val glob : string -> Regexp.t

val glob' : bool -> string -> Regexp.t
   (* Same, but allows to choose whether dots at the beginning of a
      file name need to be explicitly matched (true) or not (false) *)

val globx : string -> Regexp.t
val globx' : bool -> string -> Regexp.t
    (* These two functions also recognize the pattern {..,..} *)
