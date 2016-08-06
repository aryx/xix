(* Copyright 2016 Yoann Padioleau, see copyright.txt *)
open Common

module J = Job
module G = Graph
module R = Rules
module E = Env

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Globals *)
(*****************************************************************************)

let running = Hashtbl.create 101
let nrunning = ref 0

(* todo: use environemt to set it *)
let nproclimit = ref 2

let jobs = Queue.create ()

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* !modify by side effect job.env! *)
let adjust_env job =
  let env = job.J.env in

  (* less: should be all_target *)
  Hashtbl.replace env.E.internal_vars "target" job.J.all_targets;
  (* less: newprereqs *)
  Hashtbl.replace env.E.internal_vars "prereq" job.J.all_prereqs;
  job.J.rule.Rules.stem |> Common.if_some (fun s ->
    Hashtbl.replace env.E.internal_vars "stem" [s];
  );

  env

let shprint env s =
  let s = 
    Str.global_substitute (Str.regexp "\\$\\([a-zA-Z][a-zA-Z0-9_]*\\)")
      (fun _wholestr ->
        let var = Str.matched_group 1 s in
        if Hashtbl.mem env.E.internal_vars var
        then Hashtbl.find env.E.internal_vars var |> String.concat " "
        else 
          if Hashtbl.mem env.E.vars_we_set var
          then Hashtbl.find env.E.vars var |> String.concat " "
          else Str.matched_string s
      ) s
  in
  print_string ("|" ^ s ^ "|\n");
  (* bug: dont forget to flush, otherwise can have weird output
   * when mix printing on stdout and stderr like printing
   * multiple times the same recipe (weird, very weird)
   *)
  flush stdout

(*****************************************************************************)
(* Debug *)
(*****************************************************************************)

let dump_job func job pidopt =
  pr2 (spf "(%d): %s pid = %d; targets = %s" 
         (Unix.getpid())
         func 
         (match pidopt with Some pid -> pid | None -> 0)
         (job.J.target_nodes |> List.map (fun node -> node.G.name) 
             |> String.concat " "))
(*
  let rule = job.J.rule in
      ; recipe = '%s'; stem = %s
         (match rule.R.recipe2 with 
         | None -> "" 
         | Some (Ast.R xs) ->String.concat "\\n" xs
         )
         (match rule.R.stem with
         | None -> ""
         | Some s -> s
         )
*)


(*****************************************************************************)
(* Main algorithms *)
(*****************************************************************************)

let sched () =
  try 
    let job = Queue.take jobs in

    if !Flags.dump_jobs
    then dump_job "sched1: firing up " job None;

    let recipe = 
      match job.J.rule.R.recipe2 with 
      | Some (Ast.R x) -> x
      | None -> raise (Impossible "job without a recipe")
    in
    let env = adjust_env job in
    
    (* less: unless Quiet *)
    recipe |> List.iter (fun s -> shprint env s);

    if !Flags.dry_mode 
    then job.J.target_nodes |> List.iter (fun node ->
      node.G.time <- Some (Unix.time ());
      node.G.state <- G.Made;
    )
    else begin
      let flags = ["-e"] in
      let shellenv = Env.shellenv_of_env env in
      let pid = Shell.execsh shellenv flags recipe in

      if !Flags.dump_jobs
      then dump_job "sched2: " job (Some pid);

      Hashtbl.add running pid job;
      incr nrunning
    end
    
  with Queue.Empty ->
    raise (Impossible "no jobs to schedule")


(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

let run job =
  Queue.add job jobs;

  if !Flags.dump_jobs
  then dump_job "run: " job None;

  if !nrunning < !nproclimit
  then sched ()



let waitup () =
  let (pid, ret) = 
    try 
      Unix.wait () 
    with Unix.Unix_error (error, str1, str2) ->
      failwith (spf "%s: %s (%s)" str1 (Unix.error_message error) str2)
  in
  let job = 
    try Hashtbl.find running pid
    with Not_found ->
      raise 
        (Impossible (spf "wait returned unexpected process with pid %d" pid))
  in
  if !Flags.dump_jobs
  then dump_job "waitup: " job (Some pid);

  Hashtbl.remove running pid;
  decr nrunning;

  match ret with
  | Unix.WEXITED 0 ->
      job.J.target_nodes |> List.iter (fun node ->
        G.update node
      );
      (* similar code in run();
       * I added the test on jobs size though.
      *)
      if !nrunning < !nproclimit && Queue.length jobs > 0
      then sched ()
  | Unix.WEXITED n ->
      failwith (spf "error in child process, exit status = %d" n)
  | Unix.WSIGNALED n | Unix.WSTOPPED n ->
      failwith (spf "child process killed/stopped by signal = %d" n)
