(* will preprocess the code internally first *)
val parse: 
  < Cap.open_in; .. > -> Preprocessor.conf -> Fpath.t -> Ast_asmi.program

val parse_no_cpp: Chan.i -> Ast_asmi.program
