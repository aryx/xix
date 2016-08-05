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

let build_shellenv job =
  raise Todo

let shprint env shellenv s =
  (* TODO *)
  print_string (s ^ "\n")

let sched () =
  try 
    let job = Queue.take jobs in
    let recipe = 
      match job.J.rule.R.recipe2 with 
      | Some x -> x 
      | None -> raise (Impossible "job without a recipe")
    in

    let shellenv = build_shellenv job in
    
    (* less: unless Quiet *)
    recipe |> (fun (Ast.R xs) -> xs |> List.iter (fun s ->
      shprint job.J.env shellenv s
    ));

    if !Flags.dry_mode 
    then job.J.target_nodes |> List.iter (fun node ->
      node.G.time <- Some (Unix.time ());
      node.G.state <- G.Made;
    )
    else
      let flags = "-e" in
      let shellenv = raise Todo in
      let _pid = Shell.execsh shellenv flags recipe in
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
