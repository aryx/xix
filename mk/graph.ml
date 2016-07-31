(* Copyright 2016 Yoann Padioleau, see copyright.txt *)
open Common

module R = Rules

type node = {
  (* usually a filename *)
  name: string;
  (* None for virtual targets and inexistent files *)
  time: float option;
  
  (* todo: other flags *)
  (* is_virtual: bool; *)

  prereqs: arc list ref;
}
and arc = {
  (* can point to an existing node since the graph of dependencies is a DAG *)
  dest: node option;

  (* what we need from the rule *)
  rule_exec: Rules.rule_exec;
}

let hnode = Hashtbl.create 101

(* opti? time cache, and flag to skip cache if want refresh *)
let timeof file =
  try 
    let stat = Unix.lstat file in
    Some (stat.Unix.st_mtime)
  with Unix.Unix_error (_, _, _) -> None

let new_node target =
  let node = {
    name = target;
    time = timeof target;
    prereqs = ref []
  }
  in
  Hashtbl.add hnode target node;
  node

let exec_stem r stem =
  { (Rules.rule_exec r) with Rules.stem = Some stem }

(* todo: cycle detection, ambiguous and graph fixing *)
let check_graph root =
  pr2 "TODO: check_graph";
  root


let rec apply_rules target rules =
  if Hashtbl.mem hnode target
  then Hashtbl.find hnode target
  else begin
    let node = new_node target in
    let arcs = node.prereqs in
  
    (* look for simple rules *)
    let rs = Hashtbl.find_all rules.R.simples target in
    rs |> List.iter (fun r ->
      (* less: if no recipe and no prereqs, skip, but weird rule no? *)
      let pre = r.R.prereqs in
      if pre = []
      then arcs |> Common.push { dest = None; rule_exec = Rules.rule_exec r }
      else pre |> List.iter (fun prereq ->
        let dest = apply_rules prereq rules in
        arcs |> Common.push { dest = Some dest; rule_exec = Rules.rule_exec r }
      )
    );

    (* look for meta rules *)
    let rs = rules.R.metas in
    rs |> List.iter (fun r ->
      r.R.targets |> List.iter (fun target_pat ->
        (match Percent.match_ target_pat target with
        | None -> ()
        | Some stem ->
           (* less: if no recipe and no prereqs, skip, but weird rule no? *)
          let pre = r.R.prereqs in
          if pre = []
          then arcs |> Common.push { dest = None; rule_exec=exec_stem r stem }
          else pre |> List.iter (fun prereq_pat ->
            let prereq = Percent.subst prereq_pat stem in
            let dest = apply_rules prereq rules in
            arcs |> Common.push { dest = Some dest; rule_exec=exec_stem r stem }
          )
        )
      )
    );

    (* should not matter normally, but nice to have same sequential order 
     * of exec as the one specified in the mkfile *)
    node.prereqs := List.rev !arcs;
    node
  end
  

(* todo: infinite rule detection *)
let build_graph target rules =
  let root = apply_rules target rules in
  check_graph root;
  (* todo: propagate_attribute *)
  root
