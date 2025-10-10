
(* will call cpp internally first *)
val parse: 
  (Preprocessor.cmdline_defs * Preprocessor.include_paths)-> Fpath.t -> 
  Ast_asm5.program

val parse_no_cpp: Fpath.t -> Ast_asm5.program
