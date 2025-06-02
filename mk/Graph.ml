(*s: Graph.ml *)
(* Copyright 2016 Yoann Padioleau, see copyright.txt *)
open Stdcompat (* for |> *)
open Common
open Fpath_.Operators

module A = Ast
module R = Rules

module Set = Set_

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(*s: type [[Graph.node]] *)
type node = {
  (* usually a filename *)
  name: string;

  (* mutable because this field can be adjusted later in check_ambiguous
   * and vacuous.
   * Note that this field was called 'prereqs' before, but virtual rules
   * without any prereq still have an arc (with an empty dest,
   * and a recipe), so 'arcs' is a better field name.
   *)
  mutable arcs: arc list;
  (*s: [[Graph.node]] other fields *)
  (* None for inexistent files (and virtual targets).
   * mutable because it will be updated once the target is generated
   * (or when we discover a target is a virtual node).
   *)
  mutable time: float option;
  (*x: [[Graph.node]] other fields *)
  mutable state: build_state;
  (*x: [[Graph.node]] other fields *)
  (* used only for check_cycle for now *)
  mutable visited: bool;
  (*x: [[Graph.node]] other fields *)
  (* used for vacuous *)
  mutable probable: bool;
  (*x: [[Graph.node]] other fields *)
  mutable is_virtual: bool;
  (*e: [[Graph.node]] other fields *)
}
(*e: type [[Graph.node]] *)
(*s: type [[Graph.arc]] *)
  and arc = {
    (* note that because the graph of dependencies is a DAG, multiple
     * arcs may point to the same node. 
     *)
    dest: node option;
    (*s: [[Graph.arc]] other fields *)
    (* what we need from the rule to execute a recipe (and report errors) *)
    rule: Rules.rule_exec;
    (*e: [[Graph.arc]] other fields *)
  }
(*e: type [[Graph.arc]] *)
(*s: type [[Graph.build_state]] *)
  and build_state = 
    | NotMade
    | BeingMade
    | Made
(*e: type [[Graph.build_state]] *)

(*s: type [[Graph.t]] *)
type t = node (* the root *)
(*e: type [[Graph.t]] *)


(*s: constant [[Graph.hnodes]] *)
(* The graph is a DAG; some arcs may point to previously created nodes.
 * This is why we store in this hash all the created nodes.
 * 
 * Moreover, later in dorecipe() when we run a job, we want to build
 * the list of all target nodes concerned by a rule and this requires
 * again given a target name to find the corresponding node in the graph.
 *)
let hnodes = Hashtbl.create 101
(*e: constant [[Graph.hnodes]] *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(*s: function [[Graph.new_node]] *)
let new_node (target : string) =
  let time = File.timeof (Fpath.v target) in
  let node = {
    name = target;
    arcs = [];
    state = NotMade;
    time = time;
    visited = false;
    is_virtual = false;
    probable = time <> None;
  }
  in
  Logs.debug (fun m -> m "newnode(%s), time = %s" target 
                (File.str_of_time node.time));
  Hashtbl.add hnodes target node;
  node
(*e: function [[Graph.new_node]] *)

(*s: function [[Graph.rule_exec]] *)
let rule_exec (r: string Rules.rule) =
  { R.recipe2 = r.R.recipe;
    R.loc2 = r.R.loc;
    R.attrs2 = r.R.attrs;

    R.stem = None;
    R.all_targets = r.R.targets;
    R.all_prereqs = r.R.prereqs;
  }
(*e: function [[Graph.rule_exec]] *)

(*s: function [[Graph.rule_exec_meta]] *)
let rule_exec_meta (r: Percent.pattern Rules.rule) stem =
  { R.recipe2 = r.R.recipe;
    R.loc2 = r.R.loc;
    R.attrs2 = r.R.attrs;

    R.stem = Some stem;
    R.all_targets = r.R.targets |> List.map (fun pat -> Percent.subst pat stem);
    R.all_prereqs = r.R.prereqs |> List.map (fun pat -> Percent.subst pat stem);
  }
(*e: function [[Graph.rule_exec_meta]] *)


(*****************************************************************************)
(* Main algorithm *)
(*****************************************************************************)

(*s: function [[Graph.apply_rules]] *)
(* todo: infinite rule detection *)
let rec apply_rules target rules =
  Logs.debug (fun m -> m "apply_rules('%s')" target);

  (* the graph of dependency is a DAG, so we must look if node already there *)
  if Hashtbl.mem hnodes target
  then Hashtbl.find hnodes target
  else begin

    let node = new_node target in
    let arcs = ref [] in
  
    (* look for simple rules *)
    let rs = Hashtbl.find_all rules.R.simples target in
    rs |> List.iter (fun r ->
      node.probable <- true;

      let pre = r.R.prereqs in
      if pre = []
      then
        (* some tools (e.g., ocamldep) generate useless deps with no
         * recipe and no prereqs that we can safely skip (we could warn)
         *)
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
            * especially for a metarule, so maybe we should warn
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

    (* List.rev is optional. The order should not matter, but it can
     * be nice to have the same sequential order of exec as the one 
     * specified in the mkfile.
     *)
    node.arcs <- List.rev !arcs;
    node
  end
(*e: function [[Graph.apply_rules]] *)

(*****************************************************************************)
(* Checks *)
(*****************************************************************************)

(*s: function [[Graph.error_cycle]] *)
let error_cycle node trace =
  (* less: I could just display the loop instead of starting from root *)
  let str = 
    (node::trace) 
    |> List.rev 
    |> List.map (fun x -> 
      if x.name = node.name then spf "|%s|" x.name else x.name
     )
    |> String.concat "->"
  in
  failwith (spf "cycle in graph detected at target %s (trace = %s)"
              node.name str)
(*e: function [[Graph.error_cycle]] *)

(*s: function [[Graph.check_cycle]] *)
let check_cycle node =
  let rec aux trace node =
    (* stricter: mk also check if nodes has arcs, but looks wrong to me *)
    if node.visited
    then error_cycle node trace;

    node.visited <- true;
    node.arcs |> List.iter (fun arc ->
      arc.dest |> Common.if_some (fun node2 -> 
        aux (node::trace) node2
      )
    );
    node.visited <- false;
  in
  aux [] node
(*e: function [[Graph.check_cycle]] *)




(*s: function [[Graph.error_ambiguous]] *)
let error_ambiguous node groups =
  let candidates = 
    groups |> List.map (fun (rule, arcs) -> 
      let loc = rule.R.loc2 in
      (* one arc representative is enough *)
      let arc = List.hd arcs in
      spf "\t%s <-(%s:%d)- %s" 
        node.name !!(loc.A.file) loc.A.line
        (match arc.dest with None -> "" | Some n -> n.name)
    )
  in
  failwith (spf "ambiguous recipes for %s: \n%s" 
              node.name  (candidates |> String.concat "\n"))
(*e: function [[Graph.error_ambiguous]] *)

(*s: function [[Graph.check_ambiguous]] *)
let rec check_ambiguous node =

  node.arcs |> List.iter (fun arc ->
    (* less: opti: could use visited to avoid duplicate work in a DAG *)
    arc.dest |> Common.if_some (fun node2 -> 
      (* recurse *)
      check_ambiguous node2
    );
  );

  let arcs_with_recipe = 
    node.arcs |> List.filter (fun arc -> R.has_recipe arc.rule) in
  (* opti? rule_exec is big now, so maybe need have a rule_exec id *)
  let groups_by_rule =
    arcs_with_recipe |> Assoc.group_by (fun arc -> arc.rule)
  in
  
  match List.length groups_by_rule with
  | 0 -> ()
    (* stricter? or report it later? *)
    (* failwith (spf "no recipe to make %s" node.name) *)
  | 1 -> ()
  | 2 | _ -> 
    (* feature: it's ok to have ambiguity between 1 simple rule and 1 meta rule.
     * The specialized simple rule has priority over the generic meta rule
     * (see specialize-vs-generic in Build.nw)
     *)
    let groups_with_simple_rule =
      groups_by_rule |> List_.exclude (fun (r, _) -> R.is_meta r) 
    in
    (match List.length groups_with_simple_rule with
    | 0 -> error_ambiguous node groups_by_rule
    | 1 ->
      (* update graph *)
      node.arcs <- List_.exclude (fun arc -> R.is_meta arc.rule) node.arcs;
    | 2 | _ -> error_ambiguous node groups_with_simple_rule
    )
(*e: function [[Graph.check_ambiguous]] *)

(*****************************************************************************)
(* Adjustments *)
(*****************************************************************************)
(*s: function [[Graph.propagate_attributes]] *)
let rec propagate_attributes node =
  node.arcs |> List.iter (fun arc ->
    arc.rule.R.attrs2 |> Set.iter (function
      | A.Virtual -> 
          node.is_virtual <- true;
          (* maybe there was a file with the name of the virtual target 
           * in the directory, but we do not want its time
           *)
          node.time <- None;
      | _ -> ()
    );
    arc.dest |> Common.if_some propagate_attributes
  )
(*e: function [[Graph.propagate_attributes]] *)


(*s: function [[Graph.vacuous]] *)
let rec vacuous node =
  let vacuous_node = ref (not node.probable) in
  
  node.arcs <- node.arcs |> List_.exclude (fun arc ->
    match arc.dest with
    | Some node2 -> 
        if vacuous node2 && R.is_meta arc.rule
        then begin
          Logs.warn (fun m -> m "vacuous arc detected: %s -> %s" 
                        node.name node2.name);
          true
        end else begin 
          vacuous_node := false; 
          false 
        end
    | None ->
        vacuous_node := false;
        false
  );
  if !vacuous_node
  then Logs.warn (fun m -> m "vacuous node detected: %s" node.name);
  !vacuous_node
(*e: function [[Graph.vacuous]] *)


(*****************************************************************************)
(* Debug *)
(*****************************************************************************)

(*s: function [[Graph.loc_of_arc]] *)
let loc_of_arc arc =
  let loc = arc.rule.R.loc2 in
  spf "(%s:%d)" !!(loc.Ast.file) loc.Ast.line
(*e: function [[Graph.loc_of_arc]] *)

(*s: function [[Graph.dump_graph]] *)
let dump_graph node =
  let pr s = 
    print_string (s ^ "\n") 
  in
  pr "digraph misc {";
  pr "size = \"10,10\";" ;
  let hdone = Hashtbl.create 101 in
  let rec aux node1 =
    if Hashtbl.mem hdone node1.name
    then ()
    else begin
      Hashtbl.add hdone node1.name true;
      node1.arcs |> List.iter (fun arc ->
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
(*e: function [[Graph.dump_graph]] *)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

(*s: function [[Graph.build_graph]] *)
let build_graph target rules =
  let root = apply_rules target rules in

  check_cycle root;

  (* must be before check_ambiguous! *)
  root.probable <- true;
  vacuous root |> ignore;

  check_ambiguous root;

  propagate_attributes root;
   
  root
(*e: function [[Graph.build_graph]] *)

(*s: function [[Graph.update]] *)
(* update graph once a node has been built *)
let update node =
  node.state <- Made;
  Logs.debug (fun m -> m "update(): node %s time=%s" node.name 
                 (File.str_of_time node.time));
  
  if node.is_virtual
  then begin
    node.time <- Some 1.0;
    (* less: take max time of prereqs, need that? *)
  end
  else begin
    let oldtime = node.time in
    node.time <- File.timeof (Fpath.v node.name);

    (* todo: actually can happen for rule like
     * x.tab.h: y.tab.h
     *   cmp -s x.tab.h y.tab.h || cp y.tab.h x.tab.h
     * (see plan9/shell/rc/mkfile).
     * In that case we should not failwith.
     * Because it is a rare case, maybe we should have a special
     * attribute for those rules, so not fail only when have this
     * attribute
     *)
    if oldtime = node.time || node.time = None
    then failwith (spf "recipe did not update %s, time =%s" node.name
                     (File.str_of_time node.time));
  end
(*e: function [[Graph.update]] *)
(*e: Graph.ml *)
