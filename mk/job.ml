
type t = {
  rule: Rules.rule_exec;
  target_nodes: Graph.node list;

  all_targets: Env.values;
  all_prereqs: Env.values;
  (* less: newprereqs, targets *)
}


