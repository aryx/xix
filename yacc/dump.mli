(*s: yacc2/dump.mli *)

(*s: signature Dump.dump_item (yacc) *)
val dump_item: Lr0.env -> Lr0.item -> unit
(*e: signature Dump.dump_item (yacc) *)

(*s: signature Dump.dump_items (yacc) *)
val dump_items: Lr0.env -> Lr0.items -> unit
(*e: signature Dump.dump_items (yacc) *)

(*s: signature Dump.dump_lr0_automaton (yacc) *)
val dump_lr0_automaton: Lr0.env -> Lr0.automaton -> unit
(*e: signature Dump.dump_lr0_automaton (yacc) *)

(*s: signature Dump.dump_lrtables (yacc) *)
val dump_lrtables: Lr0.env -> Lrtables.lr_tables -> unit
(*e: signature Dump.dump_lrtables (yacc) *)
(*e: yacc2/dump.mli *)
