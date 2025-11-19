(*s: Parse.mli *)

(*s: signature [[Parse.parse]] *)
(* will macropreprocess internally first *)
val parse: 
  < Cap.open_in; .. > -> Preprocessor.conf -> Fpath.t -> Ast.program
(*e: signature [[Parse.parse]] *)

(*s: signature [[Parse.parse_no_cpp]] *)
(* internals *)
val parse_no_cpp:
  Chan.i -> Ast.program
(*e: signature [[Parse.parse_no_cpp]] *)
(*e: Parse.mli *)
