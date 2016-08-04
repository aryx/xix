(* Copyright 2016 Yoann Padioleau, see copyright.txt *)
open Common

module A = Ast
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

  (* A ref because it can be adjusted later in check_ambiguous.
   * It was called prereqs before, but actually virtual rules
   * without any prereq still have an arc, with an empty dest 
   * (and a recipe). So, arcs is a better field name.
   *)
  arcs: arc list ref;

  (* None for virtual targets and inexistent files.
   * mutable because it will be updated once the target is generated.
   *)
  mutable time: float option;
  mutable state: build_state;

  (* used only for check_cycle for now *)
  mutable visited: bool;

  (* todo: other flags *)
  (* is_virtual: bool; *)
}
  and arc = {
    (* note that because the graph of dependencies is a DAG, multiple
     * arcs may point to the same node. 
     *)
    dest: node option;
    (* what we need from the rule to execute a recipe (and report errors) *)
    rule: Rules.rule_exec;
  }
  and build_state = 
    | NotMade
    | BeingMade
    | Made

type graph = node (* the root *)


(* The graph is a DAG, some arcs may point to previously created nodes.
 * This is why we store in this hash all the created nodes.
 * 
 * Moreover, later in dorecipe() when we run a job, we want to build
 * the list of all target nodes concerned by a rule and this requires
 * again given a target name to find the corresponding node in the graph.
 *)
let hnodes = Hashtbl.create 101

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
    arcs = ref [];
    state = NotMade;
    visited = false;
  }
  in
  Hashtbl.add hnodes target node;
  node

let rule_exec (r: string Rules.rule) =
  { R. 
    recipe2 = r.R.recipe;
    stem = None;
    loc2 = r.R.loc;
    all_targets = r.R.targets;
    all_prereqs = r.R.prereqs;
  }

let rule_exec_meta (r: Percent.pattern Rules.rule) stem =
  { R. 
    recipe2 = r.R.recipe;
    stem = Some stem;
    loc2 = r.R.loc;
    all_targets = r.R.targets |> List.map (fun pat -> Percent.subst pat stem);
    all_prereqs = r.R.prereqs |> List.map (fun pat -> Percent.subst pat stem);
  }



(*****************************************************************************)
(* Main algorithm *)
(*****************************************************************************)

(* todo: infinite rule detection *)
let rec apply_rules target rules =
  (* the graph of dependency is actually a DAG, so look if node already there *)
  if Hashtbl.mem hnodes target
  then Hashtbl.find hnodes target
  else begin

    let node = new_node target in
    let arcs = node.arcs in
  
    (* look for simple rules *)
    let rs = Hashtbl.find_all rules.R.simples target in
    rs |> List.iter (fun r ->
      let pre = r.R.prereqs in
      if pre = []
      then
        (* some tools generate useless deps with no recipe and no prereqs *)
        if r.R.recipe = None
        then ()
        else arcs |> Common.push { dest = None; rule = rule_exec r }
      else pre |> List.iter (fun prereq ->
        (* recurse *)
        let dest = apply_rules prereq rules in
        arcs |> Common.push { dest = Some dest; rule = rule_exec r }
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
          then arcs |> Common.push { dest = None; rule = rule_exec_meta r stem }
          else pre |> List.iter (fun prereq_pat ->
            let prereq = Percent.subst prereq_pat stem in
            (* recurse *)
            let dest = apply_rules prereq rules in
            arcs |> Common.push { dest = Some dest; rule=rule_exec_meta r stem }
          )
        )
      )
    );

    (* should not matter normally, but nice to have same sequential order 
     * of exec as the one specified in the mkfile 
     *)
    node.arcs := List.rev !arcs;
    node
  end

(*****************************************************************************)
(* Checks *)
(*****************************************************************************)

let error_cycle node trace =
  (* less: I could just display the loop instead of starting from root *)
  let str = (node::trace) |> List.rev |> List.map (fun x -> 
    if x.name = node.name then spf "|%s|" x.name else x.name
  ) |> String.concat "->"
  in
  failwith (spf "cycle in graph detected at target %s (trace = %s)"
              node.name str)

let check_cycle node =

  let rec aux trace node =
    if node.visited
    then error_cycle node trace;

    node.visited <- true;
    !(node.arcs) |> List.iter (fun arc ->
      arc.dest |> Common.if_some (fun node2 -> 
        aux (node::trace) node2
      )
    );
    node.visited <- false;
  in
  aux [] node




let error_ambiguous node groups =
  let candidates = 
    groups |> List.map (fun (rule, arcs) -> 
      let loc = rule.R.loc2 in
      (* one arc representative is enough *)
      let arc = List.hd arcs in
      spf "\t%s <- (%s:%d)- %s" 
        node.name loc.A.file loc.A.line
        (match arc.dest with None -> "" | Some n -> n.name)
    )
  in
  failwith (spf "ambiguous recipes for %s: \n%s" 
              node.name  (candidates |> String.concat "\n"))

let rec check_ambiguous node =

  !(node.arcs) |> List.iter (fun arc ->
    (* less: opti: could use visited to avoid duplicate work in a DAG *)
    arc.dest |> Common.if_some (fun node2 -> 
      (* recurse *)
      check_ambiguous node2
    );
  );

  let arcs = !(node.arcs) in
  let arcs_with_recipe = 
    arcs |> List.filter (fun arc -> R.has_recipe arc.rule) in
  let group_by_rule =
    arcs_with_recipe |> Common.group_by (fun arc -> arc.rule)
  in
  
  match List.length group_by_rule with
  | 0 -> ()
    (* stricter? or report it later? *)
    (* failwith (spf "no recipe to make %s" node.name) *)
  | 1 -> ()
  | 2 | _ -> 
    let group_with_simple_rule =
      group_by_rule |> Common.exclude (fun (r, _) -> R.is_meta r) 
    in
    (match List.length group_with_simple_rule with
    | 0 -> error_ambiguous node group_by_rule
    | 1 ->
      (* update graph *)
      node.arcs := Common.exclude (fun arc -> R.is_meta arc.rule) arcs;
    | 2 | _ -> error_ambiguous node group_with_simple_rule
    )



(*****************************************************************************)
(* Debug *)
(*****************************************************************************)

let loc_of_arc arc =
  let loc = arc.rule.R.loc2 in
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
      !(node1.arcs) |> List.iter (fun arc ->
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
  (* todo: propagate_attribute *)
  if !Flags.dump_graph then dump_graph root;
  root
