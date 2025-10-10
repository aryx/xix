
(* will call cpp internally first *)
val parse: 
  Preprocessor.conf -> Fpath.t -> Ast.program

(* internals *)
val parse_no_cpp:
  Fpath.t -> Ast.program
