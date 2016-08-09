(*s: yacc/lr0.mli *)

(*s: type Lr0.ruleidx (yacc) *)
(* the index of the rule in env.g *)
type ruleidx = R of int
(*e: type Lr0.ruleidx (yacc) *)
(*s: type Lr0.dotidx (yacc) *)
(* the dot position in the rhs of a rule *)
type dotidx = D of int
(*e: type Lr0.dotidx (yacc) *)

(*s: type Lr0.stateid (yacc) *)
type stateid = S of int
(*e: type Lr0.stateid (yacc) *)

(*s: type Lr0.item (yacc) *)
(* as mentionned in the dragon book *)
type item = ruleidx * dotidx
(*e: type Lr0.item (yacc) *)

(*s: type Lr0.items (yacc) *)
(* a.k.a an LR0 state *)
type items = item Set_.t
(*e: type Lr0.items (yacc) *)

(*s: type Lr0.env (yacc) *)
type env = {
  (* augmented grammar where r0 is $S -> start_original_grammar *)
  g: Ast.rule_ array;
}
(*e: type Lr0.env (yacc) *)

(*s: type Lr0.automaton (yacc) *)
type automaton = {
  states: items Set_.t;
  (* state 0 is the starting state *)
  int_to_state: items array;
  state_to_int: (items, stateid) Map_.t;
  (* goto mapping *)
  trans: (items * Ast.symbol, items) Map_.t;
}
(*e: type Lr0.automaton (yacc) *)

(*s: signature Lr0.mk_env_augmented_grammar (yacc) *)
val mk_env_augmented_grammar: Ast.nonterm (* start *) -> Ast.grammar -> env
(*e: signature Lr0.mk_env_augmented_grammar (yacc) *)

(*s: signature Lr0.closure (yacc) *)
val closure: env -> items -> items
(*e: signature Lr0.closure (yacc) *)

(*s: signature Lr0.goto (yacc) *)
val goto: env -> items -> Ast.symbol -> items
(*e: signature Lr0.goto (yacc) *)

(*s: signature Lr0.canonical_lr0_automaton (yacc) *)
(* assumes augmented grammar *)
val canonical_lr0_automaton: env -> automaton
(*e: signature Lr0.canonical_lr0_automaton (yacc) *)

(* helper functions used also by slr.ml *)

(*s: signature Lr0.after_dot (yacc) *)
val after_dot: Ast.rule_ -> dotidx -> Ast.symbol option
(*e: signature Lr0.after_dot (yacc) *)

(*s: signature Lr0.all_symbols (yacc) *)
val all_symbols: env -> Ast.symbol Set_.t
(*e: signature Lr0.all_symbols (yacc) *)
(*e: yacc/lr0.mli *)
