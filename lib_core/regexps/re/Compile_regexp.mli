
type 'a match_info =
  [ `Match of 'a
  | `Failed
  | `Running ]

(* see .mli for more info *)
type state = { 
    idx : int;
    real_idx : int;
    next : state array;
    mutable final :
      (Automata.category *
       (Automata.idx * Automata.mark_infos match_info)) list;
    desc : Automata.state
}

type re = {
    initial : Automata.expr;
    mutable initial_states : (int * state) list;
    cols : string;
    col_repr : string;
    ncol : int;
    lnl : int;
    mutable tbl : Automata.working_area;
    states : state Automata.States.t;
    group_count : int
}

(* entry point *)
val compile: Regexp.t -> re

(* helpers used also in re.ml *)
type category = int

val cat_inexistant: category
val cat_newline: category
val cat_lastnewline: category
val cat_letter: category
val cat_not_letter: category
val cat_search_boundary: category
