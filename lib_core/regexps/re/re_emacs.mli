(*
   RE - A regular expression library
   Copyright (C) 2001 Jerome Vouillon
*)

(* Errors that can be raised during the parsing of the regular expression *)
exception Parse_error
exception Not_supported

(* Parsing of an Emacs-style regular expression *)
val re : ?case:bool -> string -> Re.t

(* Regular expression compilation *)
val compile : Re.t -> Re.re
      (* Same as [Re.compile] *)
val compile_pat : ?case:bool -> string -> Re.re
