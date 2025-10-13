(*s: yacc/Lrtables.ml *)

(*s: type [[Lrtables.action]](yacc) *)
type action =
  | Shift of Lr0.stateid
  | Reduce of Lr0.ruleidx
  | Accept
  | Error
(*e: type [[Lrtables.action]](yacc) *)

(*s: type [[Lrtables.action_table]](yacc) *)
(* term can be also the special "$" terminal.
 * Everything not in the list is an Error action, so this
 * list should not contain any Error.
 *)
type action_table = 
    ((Lr0.stateid * Ast.term) * action) list
(*e: type [[Lrtables.action_table]](yacc) *)

(*s: type [[Lrtables.goto_table]](yacc) *)
type goto_table = 
    ((Lr0.stateid * Ast.nonterm) * Lr0.stateid) list
(*e: type [[Lrtables.goto_table]](yacc) *)
    
(*s: type [[Lrtables.lr_tables]](yacc) *)
type lr_tables = action_table * goto_table
(*e: type [[Lrtables.lr_tables]](yacc) *)

(*e: yacc/Lrtables.ml *)
