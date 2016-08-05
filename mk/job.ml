
type t = {
  rule: Rules.rule_exec;

  env: Env.t;
  (* values for special mk variables such as $target, $prereq, etc.
   * $stem is in rule_exec.
   *)
  all_targets: Env.values;
  all_prereqs: Env.values;
  (* less: newprereqs, targets *)

  (* nodes we will need to update once the job is done *)
  target_nodes: Graph.node list;
}


