open Stdcompat (* for |> *)
open Common

module R = Runtime


let s_of_unix_error err _s1 _s2 = 
  spf "%s" (Unix.error_message err)


let exit s =
  (* todo: Updenv *)
  Status.setstatus s;
  (* todo: how communicate error to parent process under Unix? *)
  exit (if Status.truestatus () then 0 else 1)

(* Was called Xreturn but called not only from the opcode interpreter.
 * It is an helper function really.
 *)
let return () =
  R.turf_redir ();
  match !R.runq with
  | [] -> failwith "empty runq"
  (* last thread in runq, we exit then *)
  | [x] -> exit (Status.getstatus ())
  | x::xs -> 
      R.runq := xs




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
          match t.R.waitstatus with
          | R.WaitFor pid ->
              if pid = pid2
              then t.R.waitstatus <- R.ChildStatus status_str;
          | R.ChildStatus _ | R.NothingToWaitfor -> ()
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
