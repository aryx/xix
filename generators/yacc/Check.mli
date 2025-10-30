(*s: yacc/Check.mli *)

(*s: type [[Check.error]](yacc) *)
type error = unit
(*e: type [[Check.error]](yacc) *)

(*s: exception [[Check.Error]](yacc) *)
exception Error of error
(*e: exception [[Check.Error]](yacc) *)

(*s: signature [[Check.check]](yacc) *)
val check: Ast.parser_definition -> unit
(*e: signature [[Check.check]](yacc) *)
(*e: yacc/Check.mli *)
