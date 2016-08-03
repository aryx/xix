
type node = {
  name: string;
  prereqs: arc list ref;

  mutable time: float option;
  mutable state: build_state;
}
and arc = {
  dest: node option;
  rule_exec: Rules.rule_exec;
}
and build_state =
  | NotMade
  | BeingMade
  | Made

val build_graph: 
  string (* target *) -> Rules.t -> node (* the root *)
