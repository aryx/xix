(*s: Job.ml *)

(*s: type [[Job.t]] *)
type t = {
  rule: Rules.rule_exec;

  env: Env.t;
  (* values for special mk variables such as $target, $prereq, etc.
   * (note that $stem is in rule_exec)
   *)
  all_targets: Env.values;
  all_prereqs: Env.values;
  (* less: newprereqs, targets *)
  main_target: string;

  (* nodes mk needs to update once the job is done *)
  target_nodes: Graph.node list;
}
(*e: type [[Job.t]] *)
(*e: Job.ml *)
