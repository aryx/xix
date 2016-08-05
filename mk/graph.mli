
type node = {
  name: string;

  mutable arcs: arc list;

  mutable time: float option;
  mutable state: build_state;

  mutable visited: bool;

  mutable is_virtual: bool;
}
  and arc = {
    dest: node option;
    rule: Rules.rule_exec;
  }
  and build_state =
    | NotMade
    | BeingMade
    | Made

type graph = node (* the root *)

val hnodes: (string, node) Hashtbl.t

(* will also modify hnode *)
val build_graph: 
  string (* target *) -> Rules.rules -> graph


val check_cycle: 
  graph -> unit

(* will also adjust the graph *)
val check_ambiguous:
  graph -> unit


(* output graphviz dot file *)
val dump_graph: 
  graph -> unit
