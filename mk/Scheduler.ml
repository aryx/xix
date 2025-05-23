(* Copyright 2016 Yoann Padioleau, see copyright.txt *)
open Stdcompat (* for |> *)
open Common

module J = Job
module G = Graph
module R = Rules
module E = Env

module Set = Set_

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Globals *)
(*****************************************************************************)

let running = Hashtbl.create 101
let nrunning = ref 0

let nproclimit (caps: < Cap.env; ..>) =
  (try 
    let s = CapSys.getenv caps "NPROC" in
    int_of_string s
   with Not_found | _ -> 2
  )

let jobs = Queue.create ()

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* !modify by side effect job.env! *)
let adjust_env job =
  let env = job.J.env in

  (* less: should be all_target *)
  Hashtbl.replace env.E.internal_vars "target" [job.J.main_target];
  (* less: newprereqs *)
  Hashtbl.replace env.E.internal_vars "prereq" job.J.all_prereqs;
  job.J.rule.R.stem |> Common.if_some (fun s ->
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
  Logs.app (fun m -> m "|%s|" s)

(*****************************************************************************)
(* Debug *)
(*****************************************************************************)

let dump_job func job pidopt =
  Logs.debug (fun m -> m "(%d): %s pid = %d; targets = %s" 
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

let sched (caps : < Shell.caps; .. >) () =
  try 
    let job = Queue.take jobs in
    let rule = job.J.rule in

    if !Flags.dump_jobs
    then dump_job "sched1: firing up " job None;

    let recipe = 
      match rule.R.recipe2 with 
      | Some (Ast.R x) -> x
      | None -> raise (Impossible "job without a recipe")
    in
    let env = adjust_env job in
    
    if not (Set.mem Ast.Quiet rule.R.attrs2)
    then recipe |> List.iter (fun s -> shprint env s);

    if !Flags.dry_mode 
    then job.J.target_nodes |> List.iter (fun node ->
      node.G.time <- Some (Unix.time ());
      node.G.state <- G.Made;
    )
    else begin
      (* This -e is super important! so that by default the recipe is run
       * so that any error in a simple command ran from the recipe, even inside
       * a for loop, will exit the whole recipe with the error.
       *)
      let pid = 
        Shell.exec_recipe caps
          (Env.shellenv_of_env env)
          ["-e"]
          recipe 
          (Set.mem Ast.Interactive rule.R.attrs2) 
      in

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

let run (caps : < Shell.caps; .. >) job =
  Queue.add job jobs;

  if !Flags.dump_jobs
  then dump_job "run: " job None;

  if !nrunning < nproclimit caps
  then sched caps ()



let waitup (caps : < Shell.caps; .. >) () =
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
      if !nrunning < nproclimit caps && Queue.length jobs > 0
      then sched caps ()
  | Unix.WEXITED n ->
      (* less: call shprint *)
      if Set.mem Ast.Delete job.J.rule.R.attrs2 
      then 
        job.J.rule.R.all_targets |> List.iter (fun f ->
          if Sys.file_exists f
          then begin
            Logs.info (fun m -> m "deleting %s" f);
            Sys.remove f
          end
        );
      failwith (spf "error in child process, exit status = %d" n)
  (* old: Unix.WSIGNALED n | Unix.WSTOPPED n *)
  | Unix.WSIGNALED n ->
      failwith (spf "child process killed/stopped by signal = %d" n)
  | Unix.WSTOPPED n ->
      failwith (spf "child process killed/stopped by signal = %d" n)
