(* Copyright 2016 Yoann Padioleau, see copyright.txt *)
open Common

module G = Graph
module R = Rules
module J = Job

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let outofdate node arc =
  match arc.G.dest with
  | None -> raise (Impossible "should not call outofdate on nodeless arcs")
  | Some node2 -> 
      (* I use the strictly < in the C version because on modern machines 
       * many files can be created in the same second. In OCaml,
       * the time is a float with higher precision so I can use back
       * the <=.
       *)
       (match node.G.time, node2.G.time with
       | _      , None    -> raise (Impossible "inexistent dep")
       | None   , Some _  -> true
       | Some t1, Some t2 -> t1 < t2
       )


let dorecipe node did =
  let master_arc = 
    try 
      !(node.G.arcs) |> List.find (fun arc -> R.has_recipe arc.G.rule)
    with Not_found -> 
      (* todo: could be because virtual target *)
      failwith (spf "no recipe to make '%s'" node.G.name)
  in
  let master_rule = master_arc.G.rule in

  let all_targets = master_rule.R.all_targets in
  (* less: outofdate_targets (aka target) *)
  let nodes =
    all_targets |> Common.map_filter (fun target ->
      if Hashtbl.mem G.hnodes target
      then Some (Hashtbl.find G.hnodes target)
      else None
    )
  in
  (* less: newprereqs *)
  let all_prereqs =
    nodes |> List.fold_left (fun acc node ->
      !(node.G.arcs) |> List.fold_left (fun acc arc ->
        match arc.G.dest with
        | None -> acc
        | Some node -> Set.add node.G.name acc
      ) acc
    ) Set.empty
  in

  nodes |> List.iter (fun node ->
    node.G.state <- G.BeingMade
  );
  Scheduler.run { J. 
                  rule = master_rule;
                  target_nodes = nodes;
                  
                  all_targets = all_targets;
                  all_prereqs = Set.elements all_prereqs;
                };
  did := true

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

(* We could return a job list, which would be cleaner, but it is
 * more efficient to run a job as soon as we find an opportunity.
 *)
let rec work node did =
  if node.G.state = G.BeingMade
  then ()
  else 
    match !(node.G.arcs) with
    (* a leaf *)
    | [] ->
        if node.G.time = None
        then failwith (spf "don't know how to make '%s'" node.G.name)
        else node.G.state <- G.Made

    (* a node *)
    | xs ->
        let out_of_date = ref false in
        let ready = ref true in
        xs |> List.iter (fun arc ->
          match arc.G.dest with
          | Some node2 ->
              (* recurse! *)
              work node2 did;
              
              (match node2.G.state with
              | G.NotMade | G.BeingMade -> 
                  ready := false;
              | _ -> ()
              );
              if outofdate node arc 
              then out_of_date := true

          | None ->
              if node.G.time = None
              then out_of_date := true
        );
        if not !ready
        then ()
        else 
          if not !out_of_date 
          then node.G.state <- G.Made
          else dorecipe node did
