
(* will call cpp internally first *)
val parse: 
  (Preprocessor.cmdline_defs * Preprocessor.include_paths)-> Common.filename -> 
  Ast.program

(* internals *)
val parse_no_cpp:
  Common.filename -> Ast.program
