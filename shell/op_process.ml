open Common

module R = Runtime
module E = Error

let execute args cmdpath =
  raise Todo

let exec () =
  R.pop_word ();

  let t = R.cur () in
  let argv = t.R.argv in

  match argv with
  | [] -> E.error "empty argument list" 
  | prog::xs -> 
      (* todo: doredir *)
      execute argv (Path.find_in_PATH prog);
      (* should not be reached *)
      R.pop_list ()

let forkexec () =
  let pid = Unix.fork () in
  (* child *)
  if pid = 0
  then begin
    (* less: clearwaitpids *)
    R.push_word "exec";
    exec ();
    (* should not be reached *)
    R.exit "can't exec"
  end
  else 
    (* less: addwaitpid *)
    pid

let waitfor pid =
  raise Todo

let op_Simple () =
  let t = R.cur () in
  let argv = t.R.argv in

  (* less: globlist () *)
  (* less: -x *)

  match argv with
  (* How can you get an empty list as Simple has at least one word?
   * If you do A=()\n and then $A\n then Simple has a word, but after
   * expansion the list becomes empty.
   * stricter: I give extra explanations
   *)
  | [] -> E.error "empty argument list (after variable expansion)" 

  | argv0::args ->
      if Builtin.is_builtin argv0
      then Builtin.dispatch argv0 args
      else begin
        (* if exitnext opti *)
        flush stderr;
        (* less: Updenv *)
        (try 
          let pid = forkexec () in
          R.pop_list ();
          while waitfor pid <> pid do
            ()
          done
        with
          | Failure s ->
              E.error ("try again: " ^ s)
          | Unix.Unix_error (err, s1, s2) -> 
              E.error (spf "%s %s %s" (Unix.error_message err) s1 s2)
        )
      end
