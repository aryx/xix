(*s: Parse_asm5.mli *)

(*s: signature [[Parse_asm5.parse]] *)
(* will preprocess the code internally first *)
val parse: 
  < Cap.open_in; .. > -> Preprocessor.conf -> Fpath.t -> Ast_asm5.program
(*e: signature [[Parse_asm5.parse]] *)

(*s: signature [[Parse_asm5.parse_no_cpp]] *)
val parse_no_cpp: Chan.i -> Ast_asm5.program
(*e: signature [[Parse_asm5.parse_no_cpp]] *)
(*e: Parse_asm5.mli *)
