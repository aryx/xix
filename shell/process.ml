open Common

module R = Runtime


let s_of_unix_error err _s1 _s2 = 
  spf "%s" (Unix.error_message err)


type waitfor_result =
  | WaitforInterrupted
  | WaitforFound
  | WaitforNotfound

let waitfor pid =
  (* less: check for havewaitpid *)

  try 
    let rec loop () =
      let (pid2, status) = Unix.wait () in
      let status_str = 
        match status with
        | Unix.WEXITED i -> spf "%d" i
        | Unix.WSIGNALED i -> spf "signaled %d" i
        | Unix.WSTOPPED i -> spf "stopped %d" i
      in
      if pid = pid2
      then begin
        Status.setstatus status_str;
        WaitforFound
      end else begin
        !R.runq |> List.iter (fun t ->
          if t.R.pid = (Some pid2)
          then begin 
            t.R.pid <- None;
            t.R.status <- status_str;
          end
        );
        loop ()
      end
    in
    loop ()
  with Unix.Unix_error (err, s1, s2) ->
    Globals.errstr := s_of_unix_error err s1 s2;
    if err = Unix.EINTR
    then WaitforInterrupted
    else WaitforNotfound
