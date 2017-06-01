open Common

module N = Plan9
module P = Protocol_9P

let main () =
  let (clients_fd, server_fd) = Unix.pipe () in
  
  let pid = Unix.fork () in
  (match pid with
  (* child, client *)
  | 0 ->
    (try 
      Plan9.mount clients_fd (-1) "/mnt/wsys" Plan9.MRepl "win1" |> ignore;
      let dir = Unix.opendir "/mnt/wsys" in
      let str = Unix.readdir dir in
      pr (spf "dir: %s" str)
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
      (match req.P.typ with
      | P.T x ->
        (match x with
        (* for Plan9.mount *)
        | P.T.Version (msize, str) ->
          let res = { req with P.typ = P.R (P.R.Version (msize, str)) } in
          pr (P.str_of_msg res);
          P.write_9P_msg res server_fd;
          ()
        | P.T.Attach (fid, afid, uname, aname) -> 
          let qid = { N.path = 0; N.vers = 0; N.typ = N.QTDir } in
          let res = { req with P.typ = P.R (P.R.Attach qid) } in
          pr (P.str_of_msg res);
          P.write_9P_msg res server_fd;

        (* for Unix.opendir *)

        (* for Unix.readdir *)

        | _ -> raise Todo
        )
      | P.R x ->
        failwith "got a response request"
      )
    done
  )

let _ =
  main ()
