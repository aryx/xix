(*s: Scheduler.ml *)
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

(*s: constant [[Scheduler.running]] *)
let running = Hashtbl.create 101
(*e: constant [[Scheduler.running]] *)
(*s: constant [[Scheduler.nrunning]] *)
let nrunning = ref 0
(*e: constant [[Scheduler.nrunning]] *)

(*s: function [[Scheduler.nproclimit]] *)
let nproclimit (caps: < Cap.env; ..>) =
  (try 
    let s = CapSys.getenv caps "NPROC" in
    int_of_string s
   with Not_found | _ -> 2
  )
(*e: function [[Scheduler.nproclimit]] *)

(*s: constant [[Scheduler.jobs]] *)
let jobs = Queue.create ()
(*e: constant [[Scheduler.jobs]] *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(*s: function [[Scheduler.adjust_env]] *)
(* !modify by side effect job.env! *)
let adjust_env job =
  let env = job.J.env in

  (* less: should be all_target *)
  Hashtbl.replace env.E.internal_vars "target" [job.J.main_target];
  (* less: newprereqs *)
  Hashtbl.replace env.E.internal_vars "prereq" job.J.all_prereqs;
  job.J.rule.R.stem |> Option.iter (fun s ->
    Hashtbl.replace env.E.internal_vars "stem" [s];
  );

  env
(*e: function [[Scheduler.adjust_env]] *)

(*s: function [[Scheduler.shprint]] *)
let shprint env s =
  let s = 
    s |> Str.global_substitute (Str.regexp "\\$\\([a-zA-Z][a-zA-Z0-9_]*\\)")
      (fun _wholestr ->
        let var = Str.matched_group 1 s in
        match () with
        | _ when Hashtbl.mem env.E.internal_vars var ->
            Hashtbl.find env.E.internal_vars var |> String.concat " "
        | _ when Hashtbl.mem env.E.vars_we_set var ->
            Hashtbl.find env.E.vars var |> String.concat " "
        | _ -> Str.matched_string s
      )
  in
  Logs.app (fun m -> m "|%s|" s)
(*e: function [[Scheduler.shprint]] *)

(*****************************************************************************)
(* Debug *)
(*****************************************************************************)

(*s: function [[Scheduler.dump_job]] *)
let dump_job func job pidopt =
  Logs.debug (fun m -> m "(%d): %s pid = %d; targets = %s" 
         (Unix.getpid())
         func 
         (match pidopt with Some pid -> pid | None -> 0)
         (job.J.target_nodes |> List.map (fun node -> node.G.name) 
             |> String.concat " "))
(*e: function [[Scheduler.dump_job]] *)
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

(*s: function [[Scheduler.sched]] *)
let sched (caps : < Shell.caps; .. >) () =
  try 
    let job = Queue.take jobs in
    (*s: [[Scheduler.sched()]] possibly dump job *)
    if !Flags.dump_jobs
    then dump_job "sched1: firing up " job None;
    (*e: [[Scheduler.sched()]] possibly dump job *)
    let rule = job.J.rule in
    let recipe : string list = 
      match rule.R.recipe2 with 
      | Some (Ast.R x) -> x
      | None -> raise (Impossible "job without a recipe")
    in
    let env = adjust_env job in

    (*s: [[Scheduler.sched()]] guard to display the recipe *)
    if not (Set.mem Ast.Quiet rule.R.attrs2)
    (*e: [[Scheduler.sched()]] guard to display the recipe *)
    then recipe |> List.iter (fun s -> shprint env s);

    (*s: [[Scheduler.sched()]] if dry mode *)
    if !Flags.dry_mode 
    then job.J.target_nodes |> List.iter (fun node ->
      node.G.time <- Some (Unix.time ());
      node.G.state <- G.Made;
    )
    (*e: [[Scheduler.sched()]] if dry mode *)
    else begin
      (*s: [[Scheduler.sched()]] let [[interactive]] *)
      let interactive = Set.mem Ast.Interactive rule.R.attrs2 in
      (*e: [[Scheduler.sched()]] let [[interactive]] *)
      (* This -e is super important! so that by default the recipe is run
       * so that any error in a simple command ran from the recipe, even inside
       * a for loop, will exit the whole recipe with the error.
       *)
      let pid = 
        Shell.exec_recipe caps (Env.shellenv_of_env env) ["-e"] recipe interactive
      in
      (*s: [[Scheduler.sched()]] possibly dump job after [[exec_recipe]] *)
      if !Flags.dump_jobs
      then dump_job "sched2: " job (Some pid);
      (*e: [[Scheduler.sched()]] possibly dump job after [[exec_recipe]] *)
      Hashtbl.add running pid job;
      incr nrunning
    end
    
  with Queue.Empty ->
    raise (Impossible "no jobs to schedule")
(*e: function [[Scheduler.sched]] *)

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

(*s: function [[Scheduler.run]] *)
let run (caps : < Shell.caps; .. >) (job : Job.t) : unit =
  Queue.add job jobs;
  (*s: [[Scheduler.run]] possibly dump the job *)
  if !Flags.dump_jobs
  then dump_job "run: " job None;
  (*e: [[Scheduler.run]] possibly dump the job *)
  if !nrunning < nproclimit caps
  then sched caps ()
(*e: function [[Scheduler.run]] *)

(*s: function [[Scheduler.waitup]] *)
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
  (*s: [[Scheduler.waitup()]] possibly dump job *)
  if !Flags.dump_jobs
  then dump_job "waitup: " job (Some pid);
  (*e: [[Scheduler.waitup()]] possibly dump job *)
  Hashtbl.remove running pid;
  decr nrunning;

  match ret with
  (*s: [[Scheduler.waitup()]] matching [[ret]] cases *)
  | Unix.WEXITED 0 ->
      job.J.target_nodes |> List.iter (fun node ->
        G.update node
      );
      (* similar code in run();
       * I added the test on jobs size though.
      *)
      if !nrunning < nproclimit caps && Queue.length jobs > 0
      then sched caps ()
  (*x: [[Scheduler.waitup()]] matching [[ret]] cases *)
  | Unix.WEXITED n ->
      (*s: [[Scheduler.waitup()]] job exited with error code [[n]], if [[Delete]] rule *)
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
      (*e: [[Scheduler.waitup()]] job exited with error code [[n]], if [[Delete]] rule *)
      failwith (spf "error in child process, exit status = %d" n)
  (*x: [[Scheduler.waitup()]] matching [[ret]] cases *)
  (* ocaml-light: Unix.WSIGNALED n | Unix.WSTOPPED n *)
  | Unix.WSIGNALED n ->
      failwith (spf "child process killed/stopped by signal = %d" n)
  | Unix.WSTOPPED n ->
      failwith (spf "child process killed/stopped by signal = %d" n)
  (*e: [[Scheduler.waitup()]] matching [[ret]] cases *)
(*e: function [[Scheduler.waitup]] *)
(*e: Scheduler.ml *)
