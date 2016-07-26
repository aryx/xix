open Common

type node = {
  (* usually a filename *)
  name: string;
  (* None for virtual targets and inexistent files *)
  time: float option;
  
  (* todo: flags *)
  is_virtual: bool;

  prereqs: arc list ref;
}
and arc = {
  (* can point to an existing node since the graph of dependencies is a DAG *)
  dest: node option;

  (* what we need from the rule *)
  rule_exec: Rules.rule_exec;
}


let graph target rules =
  raise Todo

  
