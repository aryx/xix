
type t = {
  rule_exec: Rules.rule_exec;
  targets: Graph.node list;

  target: Env.values;
  prereqs: Env.values;
  (* less: newprereqs, alltargets *)
}


