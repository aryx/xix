open Common


let s_of_unix_error err _s1 _s2 = 
  spf "%s" (Unix.error_message err)


type waitfor_result =
  | WaitforInterrupted
  | WaitforFound
  | WaitforNotfound

let waitfor pid =
  (* less: check for havewaitpid *)

  try 
    let rec aux () =
      let (pid2, status) = Unix.wait () in
      if pid = pid2
      then begin
        Status.setstatus 
          (match status with
          | Unix.WEXITED i -> spf "%d" i
          | Unix.WSIGNALED i -> spf "signaled %d" i
          | Unix.WSTOPPED i -> spf "stopped %d" i
          );
        WaitforFound
      end else begin
        (* todo: lookup in runq for one waiting on pid *)
        aux
      end ()
    in
    aux ()
  with Unix.Unix_error (err, s1, s2) ->
    Globals.errstr := s_of_unix_error err s1 s2;
    if err = Unix.EINTR
    then WaitforInterrupted
    else WaitforNotfound
