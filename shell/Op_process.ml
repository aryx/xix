(*s: Op_process.ml *)
open Stdcompat (* for |> *)

module R = Runtime
module E = Error

(*s: function [[Op_process.execute]] *)
let execute (caps : <Cap.exec; ..>) args path =

  let argv = Array.of_list args in
  let errstr = ref "" in

  (* less: Updenv () *)
  path |> List.iter (fun root ->
    let path = (if root = "" then "" else root ^ "/") ^ argv.(0) in
    try 
      CapUnix.execv caps path argv |> ignore
    with Unix.Unix_error (err, s1, s2) ->
     errstr := Process.s_of_unix_error err s1 s2;
     Globals.errstr := s2
  );
  (* reached only when could not find a path *)
  Logs.err (fun m -> m "%s: %s" argv.(0) !errstr)
(*e: function [[Op_process.execute]] *)

(*s: function [[Op_process.exec]] *)
let exec (caps : < Cap.exec; Cap.exit; .. >) () : unit =
  R.pop_word (); (* "exec" *)

  let t = R.cur () in
  let argv = t.R.argv in
  match argv with
  | [] -> E.error caps "empty argument list" 
  | prog::_xs -> 
      (*s: [[Op_process.exec()]] before [[execute]] *)
      R.doredir t.R.redirections;
      (*e: [[Op_process.exec()]] before [[execute]] *)
      execute caps argv (PATH.search_path_for_cmd prog);
      (* should not be reached, unless prog could not be executed *)
      R.pop_list ()
(*e: function [[Op_process.exec]] *)

(*s: function [[Op_process.forkexec]] *)
let forkexec (caps : < Cap.fork; Cap.exec; Cap.exit; .. >) () : int =
  let pid = CapUnix.fork caps () in
  (* child *)
  if pid = 0
  then begin
    (* less: clearwaitpids *)
    (* less: could simplify and remove this word if exec was not a builtin *)
    R.push_word "exec";
    exec caps ();
    (* should not be reached, unless prog could not be executed *)
    Process.exit caps ("can't exec: " ^ !Globals.errstr);
    0
  end
  else 
    (* parent *)
    (* less: addwaitpid *)
    pid
(*e: function [[Op_process.forkexec]] *)

(*s: function [[Op_process.op_Simple]] *)
let op_Simple (caps : < Cap.fork; Cap.exec; Cap.chdir; Cap.exit; ..>) () =
  let t = R.cur () in
  let argv = t.R.argv in
  (*s: [[Op_process.op_Simple()]] possibly dump command *)
  (* less: globlist () *)
  if !Flags.xflag 
  then Logs.app (fun m -> m "%s" (String.concat " " argv));
  (*e: [[Op_process.op_Simple()]] possibly dump command *)
  match argv with
  (*s: [[Op_process.op_Simple()]] match [[argv]] empty case *)
  (* How can you get an empty list as Simple has at least one word?
   * If you do A=()\n and then $A\n then Simple has a word, but after
   * expansion the list becomes empty.
   * stricter: I give extra explanations
   *)
  | [] -> E.error caps "empty argument list (after variable expansion)" 
  (*e: [[Op_process.op_Simple()]] match [[argv]] empty case *)
  | argv0::args ->
      match argv0 with
      (*s: [[Op_process.op_Simple()]] match [[argv0]] builtin cases *)
      | s when Builtin.is_builtin s -> Builtin.dispatch caps argv0
      (*x: [[Op_process.op_Simple()]] match [[argv0]] builtin cases *)
      (* todo: if argv0 is a function *)
      | "builtin" -> 
          (match args with
          | [] ->
              Logs.err (fun m -> m "builtin: empty argument list");
              Status.setstatus "empty arg list";
              R.pop_list ()
          | argv0::_args ->
              R.pop_word ();
              Builtin.dispatch caps argv0
          )
      (*e: [[Op_process.op_Simple()]] match [[argv0]] builtin cases *)
      | _ ->
        (*s: [[Op_process.op_Simple()]] when default case, before the fork *)
        (* if exitnext opti *)
        flush stderr;
        (* less: Updenv *)
        (*e: [[Op_process.op_Simple()]] when default case, before the fork *)
        (try 
          let pid = forkexec caps () in
          R.pop_list ();
          (*s: [[Op_process.op_Simple()]] when default case, after the fork *)
          (* do again even if was interrupted *)
          while Process.waitfor pid = Process.WaitforInterrupted do
            ()
          done
          (*e: [[Op_process.op_Simple()]] when default case, after the fork *)
        with
          | Failure s ->
              E.error caps ("try again: " ^ s)
          | Unix.Unix_error (err, s1, s2) -> 
              E.error caps (Process.s_of_unix_error err s1 s2)
)
(*e: function [[Op_process.op_Simple]] *)
(*e: Op_process.ml *)
