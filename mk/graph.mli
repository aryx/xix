
type node = {
  name: string;
  prereqs: arc list ref;

  mutable time: float option;
  mutable visited: bool;
  mutable state: build_state;
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



val build_graph: 
  string (* target *) -> Rules.t -> graph


val check_cycle: 
  graph -> unit

(* will also adjust the graph *)
val check_ambiguous:
  graph -> unit


(* output graphviz dot file *)
val dump_graph: 
  graph -> unit
