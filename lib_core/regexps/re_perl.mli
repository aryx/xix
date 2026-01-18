(*
   RE - A regular expression library
   Copyright (C) 2001 Jerome Vouillon
*)

(* Errors that can be raised during the parsing of the regular expression *)
exception Parse_error
exception Not_supported

type opt =
  | Ungreedy | Dotall | Dollar_endonly
  | Multiline | Anchored | Caseless

(* Parsing of a Perl-style regular expression *)
val re : (*?opts:*)opt list -> string -> Regexp.t

(* Regular expression compilation *)
val compile : Regexp.t -> Regexp.re
      (* (Same as [Re.compile]) *)
val compile_pat : (*?opts:*)opt list -> string -> Regexp.re
