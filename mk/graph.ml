(* Copyright 2016 Yoann Padioleau, see copyright.txt *)
open Common

module R = Rules

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type node = {
  (* usually a filename *)
  name: string;

  prereqs: arc list ref;

  (* None for virtual targets and inexistent files.
   * mutable because it will be updated once the target is generated.
   *)
  mutable time: float option;
  mutable state: build_state;
  (* todo: other flags *)
  (* is_virtual: bool; *)
}
and arc = {
  (* note that because the graph of dependencies is a DAG, multiple arcs
   * may point to the same node.
   *)
  dest: node option;
  (* what we need from the rule *)
  rule_exec: Rules.rule_exec;
}
and build_state = 
  | NotMade
  | BeingMade
  | Made

let hnode = Hashtbl.create 101

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

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
    prereqs = ref [];
    state = NotMade;
  }
  in
  Hashtbl.add hnode target node;
  node

let exec_stem r stem =
  { (Rules.rule_exec r) with Rules.stem = Some stem }

(*****************************************************************************)
(* Checks *)
(*****************************************************************************)

(* todo: cycle detection, ambiguous and graph fixing *)
let check_graph root =
  pr2 "TODO: check_graph";
  root

(*****************************************************************************)
(* Main algorithm *)
(*****************************************************************************)

let rec apply_rules target rules =
  (* the graph of dependency is actually a DAG, so look if node already there *)
  if Hashtbl.mem hnode target
  then Hashtbl.find hnode target
  else begin

    let node = new_node target in
    let arcs = node.prereqs in
  
    (* look for simple rules *)
    let rs = Hashtbl.find_all rules.R.simples target in
    rs |> List.iter (fun r ->
      let pre = r.R.prereqs in
      if pre = []
      then
        (* some tools generate useless deps with no recipe and no prereqs *)
        if r.R.recipe = None
        then ()
        else arcs |> Common.push { dest = None; rule_exec = Rules.rule_exec r }
      else pre |> List.iter (fun prereq ->
        (* recurse *)
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
           (* less: if no recipe and no prereqs, skip, but weird rule no?
            * especially for a metarule
            *)
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

(*****************************************************************************)
(* Debug *)
(*****************************************************************************)

let loc_of_arc arc =
  let rexec = arc.rule_exec in
  let loc = rexec.R.loc2 in
  spf "(%s:%d)" loc.Ast.file loc.Ast.line

let dump_graph node =
  let pr s = print_string (s ^ "\n") in
  pr "digraph misc {";
  pr "size = \"10,10\";" ;
  let hdone = Hashtbl.create 101 in
  let rec aux node1 =
    if Hashtbl.mem hdone node1.name
    then ()
    else begin
      Hashtbl.add hdone node1.name true;
      !(node1.prereqs) |> List.iter (fun arc ->
        match arc.dest with
        | None -> pr (spf "\"%s\" -> \"<NOTHING>\";    # %s" 
                        node1.name (loc_of_arc arc))
        | Some node2 -> 
          pr (spf "\"%s\" -> \"%s\";       # %s" 
                node1.name node2.name (loc_of_arc arc));
          (* recurse *)
          aux node2
      )
    end
  in
  aux node;
  pr "}";
  ()

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

(* todo: infinite rule detection *)
let build_graph target rules =
  let root = apply_rules target rules in
  (* todo: check_graph root; *)
  (* todo: propagate_attribute *)
  if !Flags.dump_graph then dump_graph root;
  root
