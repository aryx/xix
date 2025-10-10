
(* will call cpp internally first *)
val parse: 
  (Preprocessor.cmdline_defs * Preprocessor.include_paths)-> Fpath.t -> 
  Ast.program

(* internals *)
val parse_no_cpp:
  Fpath.t -> Ast.program
