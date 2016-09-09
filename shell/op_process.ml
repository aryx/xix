open Common

module R = Runtime
module E = Error


let doredir xs =
  xs |> List.rev |> List.iter (fun redir ->
    match redir with
    | R.FromTo (xfrom, xto) ->
        Unix.dup2 xfrom xto;
        Unix.close xfrom
  )


let execute args path =

  let argv = Array.of_list args in
  let errstr = ref "" in

  (* less: Updenv () *)
  path |> List.iter (fun root ->
    let path = (if root = "" then "" else root ^ "/") ^ argv.(0) in
    try 
      Unix.execv path argv |> ignore
    with Unix.Unix_error (err, s1, s2) ->
     errstr := Process.s_of_unix_error err s1 s2;
     Globals.errstr := s2
  );
  (* reached only when could not find a path *)
  pr2 (spf "%s: %s" argv.(0) !errstr)


let exec () =
  R.pop_word (); (* "exec" *)

  let t = R.cur () in
  let argv = t.R.argv in

  match argv with
  | [] -> E.error "empty argument list" 
  | prog::xs -> 
      doredir t.R.redirections;
      execute argv (Path.search_path_for_cmd prog);
      (* should not be reached, unless prog could not be executed *)
      R.pop_list ()

let forkexec () =
  let pid = Unix.fork () in
  (* child *)
  if pid = 0
  then begin
    (* less: clearwaitpids *)
    (* less: could simplify and remove this word if exec was not a builtin *)
    R.push_word "exec";
    exec ();
    (* should not be reached, unless prog could not be executed *)
    Process.exit ("can't exec: " ^ !Globals.errstr);
    0
  end
  else 
    (* less: addwaitpid *)
    pid



let op_Simple () =
  let t = R.cur () in
  let argv = t.R.argv in

  (* less: globlist () *)
  if !Flags.xflag 
  then pr2 (String.concat " " argv);

  match argv with
  (* How can you get an empty list as Simple has at least one word?
   * If you do A=()\n and then $A\n then Simple has a word, but after
   * expansion the list becomes empty.
   * stricter: I give extra explanations
   *)
  | [] -> E.error "empty argument list (after variable expansion)" 

  | argv0::args ->
      match argv0 with
      (* todo: if argv0 is a function *)
      | "builtin" -> 
          (match args with
          | [] ->
              pr2 "builtin: empty argument list";
              Status.setstatus "empty arg list";
              R.pop_list ()
          | argv0::args ->
              R.pop_word ();
              Builtin.dispatch argv0
          )
      | s when Builtin.is_builtin s -> Builtin.dispatch argv0
      | _ ->
        (* if exitnext opti *)
        flush stderr;
        (* less: Updenv *)
        (try 
          let pid = forkexec () in
          R.pop_list ();
          (* do again even if was interrupted *)
          while Process.waitfor pid = Process.WaitforInterrupted do
            ()
          done
        with
          | Failure s ->
              E.error ("try again: " ^ s)
          | Unix.Unix_error (err, s1, s2) -> 
              E.error (Process.s_of_unix_error err s1 s2)
        )
