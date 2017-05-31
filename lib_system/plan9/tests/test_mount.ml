open Common

module P = Protocol_9P

let main () =
  let (clients_fd, server_fd) = Unix.pipe () in
  
  let pid = Unix.fork () in
  (match pid with
  (* child, client *)
  | 0 ->
    (try 
      Plan9.mount clients_fd (-1) "/mnt/wsys" Plan9.MRepl "win1" |> ignore
    with Plan9.Plan9_error (cmd, str) ->
      pr (spf "exn: %s, %s" cmd str);
    ); 
    while true do
      for i = 0 to 100 do
        ()
      done
    done
  (* parent *)
  | pid ->
    while true do
      let req = P.read_9P_msg server_fd in
      pr (P.str_of_msg req);
      (match req.P.msg with
      | P.Request (P.Q.Version (msize, str)) ->
        let res = { req with P.msg = P.Response (P.R.Version (msize, str)) } in
        P.write_9P_msg res server_fd;
        ()
      | P.Request (P.Q.Attach (afid, uname, aname)) -> 
        pr "HERE";
        raise Todo
      )
    done
  )

let _ =
  main ()
