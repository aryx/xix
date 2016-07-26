(*s: yacc2/slr.mli *)

(*s: signature Slr.lr_tables (yacc) *)
val lr_tables: 
  Lr0.env -> Lr0.automaton -> First_follow.follow -> Lrtables.lr_tables
(*e: signature Slr.lr_tables (yacc) *)
(*e: yacc2/slr.mli *)
