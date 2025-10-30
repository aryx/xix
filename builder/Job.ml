(*s: mk/Job.ml *)

(*s: type [[Job.t]] *)
type t = {
  rule: Rules.rule_exec;
  env: Env.t;

  (* nodes mk needs to update once the job is done *)
  target_nodes: Graph.node list;

  (*s: [[Job.t]] other fields *)
  (* less: newprereqs, targets *)
  main_target: string;
  (*x: [[Job.t]] other fields *)
  (* values for special mk variables such as $target, $prereq, etc.
   * (note that $stem is in rule_exec)
   *)
  all_targets: Env.values;
  all_prereqs: Env.values;
  (*e: [[Job.t]] other fields *)
}
(*e: type [[Job.t]] *)
(*e: mk/Job.ml *)
