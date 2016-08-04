(* Copyright 2016 Yoann Padioleau, see copyright.txt *)
open Common

module J = Job
module G = Graph
module R = Rules

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Globals *)
(*****************************************************************************)

let nrunning = ref 0

(* todo: use environemt to set it *)
let nproclimit = ref 2

let jobs = Queue.create ()

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let sched () =
  try 
    let job = Queue.take jobs in
    (*todo: let shellenv = build_shell_env job in *)
    
    (* less: unless Quiet *)
    job.J.rule.R.recipe2 |> Common.if_some (fun (Ast.R recipe) ->
      (* todo: subst variable!! *)
      recipe |> List.iter (fun s ->
        print_string (s ^ "\n")
      )
    );
    
    if !Flags.dry_mode 
    then job.J.target_nodes |> List.iter (fun node ->
      node.G.time <- Some (Unix.time ());
      node.G.state <- G.Made;
    )
    else
      let flags = "-e" in
      let pid = execsh flags job.recipe shellenv in
      raise Todo
    
  with Queue.Empty ->
    raise (Impossible "no jobs to schedule")

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

let run job =
  Queue.add job jobs;
  if !nrunning < !nproclimit
  then sched ()

let waitup () =
  raise Todo

