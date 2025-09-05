(*s: mk/Outofdate.ml *)
(* Copyright 2016 Yoann Padioleau, see copyright.txt *)
open Common

module G = Graph
module R = Rules
module J = Job

open Job (* for the fields *)

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(*s: function [[Outofdate.outofdate]] *)
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
       | Some t1, Some t2 -> t1 < t2
       (* in foo.exe: foo.o: foo.c, foo.exe and foo.o might not
        * exist and so have a time set to None. In that case,
        * I consider foo.exe also out of date as anyway foo.o
        * will be generated
        *)
       | _      , None    -> true
       | None   , Some _  -> true
       )
(*e: function [[Outofdate.outofdate]] *)

(*s: function [[Outofdate.opt0]] *)
let opt0 opttime =
  match opttime with
  | None -> 0.
  | Some x -> x
(*e: function [[Outofdate.opt0]] *)

(*s: function [[Outofdate.dorecipe]] *)
let dorecipe (caps : < Cap.fork; Cap.exec; .. >) env node (did : bool ref) : unit =
  if not (node.G.arcs |> List.exists (fun arc -> R.has_recipe arc.G.rule))
  then
    (*s: [[Outofdate.dorecipe()]] when no arcs with a recipe, if virtual node *)
    if node.G.is_virtual
    then G.update node
    (*e: [[Outofdate.dorecipe()]] when no arcs with a recipe, if virtual node *)
    else failwith (spf "no recipe to make '%s'" node.G.name)
  else begin
    let master_arc = 
      try node.G.arcs |> List.find (fun arc -> R.has_recipe arc.G.rule)
      with Not_found -> raise (Impossible "List.exists above")
    in
    let master_rule = master_arc.G.rule in
    let all_targets = master_rule.R.all_targets in
    (* less: outofdate_targets (aka target) *)
    (*s: [[Outofdate.dorecipe()]] compute target [[nodes]] *)
    let nodes =
      all_targets |> List.filter_map (fun target ->
        if Hashtbl.mem G.hnodes target
        then Some (Hashtbl.find G.hnodes target)
        (* TODO? failwith? when can this happen? *)
        else None
      )
    in
    (*e: [[Outofdate.dorecipe()]] compute target [[nodes]] *)
    (*s: [[Outofdate.dorecipe()]] compute [[all_prereqs]] *)
    (* less: newprereqs *)
    let all_prereqs = ref [] in
    (* bug: can not use Set for the accumulator below! Indeed, we want
     * the order for prereqs to be the original order in the mkfile.
     * If not, some command may not work if the order of the file
     * matters (e.g., with the ocaml linker the .cmo must be given
     * in a certain order)
     *)
    let hdone_prereqs = Hashtbl.create 101 in
    nodes |> List.iter (fun target ->
      target.G.arcs |> List.iter (fun arc ->
        arc.G.dest |> Option.iter (fun prereq ->
            (*s: [[Outofdate.dorecipe()]] when compute [[all_prereqs]] if explain mode *)
            (* less: could do that instead in work when find out about
             * an outofdate arc
             *)
            if !Flags.explain_mode && outofdate node arc
            then Logs.info (fun m -> m "%s(%.1f) < %s(%.1f)"
                        node.G.name (opt0 node.G.time)
                        prereq.G.name (opt0 prereq.G.time));
            (*e: [[Outofdate.dorecipe()]] when compute [[all_prereqs]] if explain mode *)
            if Hashtbl.mem hdone_prereqs prereq
            then ()
            else begin
              Hashtbl.add hdone_prereqs prereq true;
              Stack_.push prereq.G.name all_prereqs
            end
      )
    ));
    (*e: [[Outofdate.dorecipe()]] compute [[all_prereqs]] *)
    nodes |> List.iter (fun node -> node.G.state <- G.BeingMade);
    let job = { J. 
        rule = master_rule;
        env = env;
        main_target = node.G.name;
        target_nodes = nodes;
        all_targets = all_targets;
        all_prereqs = List.rev !all_prereqs;
    } in
    Scheduler.run caps job;
    did := true
  end
(*e: function [[Outofdate.dorecipe]] *)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

(*s: function [[Outofdate.work]] *)
(* alt: we could return a job list, which would be cleaner, but it is
 * more efficient to run a job as soon as we find an opportunity.
 *)
let rec work (caps: < Cap.fork; Cap.exec; .. >) env node (did : bool ref) : unit =
  Logs.debug (fun m -> m "work(%s) time=%s" node.G.name (File.str_of_time node.G.time));

  if node.G.state = G.BeingMade
  then ()
  else 
    match node.G.arcs with
    (* a leaf *)
    | [] ->
        (*s: [[Outofdate.work()]] sanity check node time when leaf node *)
        (* could be a virtual node, but weird to have a virtual node leaf *)
        if node.G.time = None
        then failwith (spf "don't know how to make '%s'" node.G.name);
        (*e: [[Outofdate.work()]] sanity check node time when leaf node *)
        (* less: why not call update here? *)
        node.G.state <- G.Made

    (* a node *)
    | xs ->
        let out_of_date = ref false in
        let ready = ref true in

        xs |> List.iter (fun arc ->
          match arc.G.dest with
          (*s: [[Outofdate.work()]] when iterating arc and matching [[arc.dest]] cases *)
          | Some node2 ->
              (* TODO: why recurse if node is Made? *)
              (* recurse! *)
              work caps env node2 did;
    
              (match node2.G.state with
              | G.NotMade | G.BeingMade -> ready := false;
              | G.Made -> ()
              );

              if outofdate node arc
              then out_of_date := true
          (*x: [[Outofdate.work()]] when iterating arc and matching [[arc.dest]] cases *)
          | None ->
              if node.G.time = None
              then out_of_date := true
          (*e: [[Outofdate.work()]] when iterating arc and matching [[arc.dest]] cases *)
        );
        if not !ready
        then ()
        else 
          if !out_of_date 
          then dorecipe caps env node did
          else node.G.state <- G.Made
(*e: function [[Outofdate.work]] *)
(*e: mk/Outofdate.ml *)
