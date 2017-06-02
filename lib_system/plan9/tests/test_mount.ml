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
      pr (spf "dir1: %s" str);
      let str = Unix.readdir dir in
      pr (spf "dir2: %s" str);
    with 
      | Plan9.Plan9_error (cmd, str) ->
        pr (spf "exn: %s, %s" cmd str);
      | Unix.Unix_error (err, cmd, args) ->
        pr (spf "exn: %s (%s, %s)" (Unix.error_message err) cmd args)
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
        | P.T.Attach (rootfid, afid, uname, aname) -> 
          let qid = { N.path = 0; N.vers = 0; N.typ = N.QTDir } in
          let res = { req with P.typ = P.R (P.R.Attach qid) } in
          pr (P.str_of_msg res);
          P.write_9P_msg res server_fd;

        (* for Unix.opendir *)
        | P.T.Walk (rootfid, new_fid, []) -> 
          let res = { req with P.typ = P.R (P.R.Walk []) } in
          pr (P.str_of_msg res);
          P.write_9P_msg res server_fd;

        | P.T.Stat (new_fid) -> 
          let qid = { N.path = 0; N.vers = 0; N.typ = N.QTDir } in
          let entry = N.mk_dir_entry "/" qid (0o500, N.DMDir) in
          let res = { req with P.typ = P.R (P.R.Stat entry) } in
          pr (P.str_of_msg res);
          P.write_9P_msg res server_fd;

        | P.T.Clunk (new_fid) -> 
          let res = { req with P.typ = P.R (P.R.Clunk ()) } in
          pr (P.str_of_msg res);
          P.write_9P_msg res server_fd;

        (* for Unix.readdir *)
        | P.T.Open (new_fid, flag) ->
          let qid = { N.path = 0; N.vers = 0; N.typ = N.QTDir } in
          let iounit = 8192 in
          let res = { req with P.typ = P.R (P.R.Open (qid, iounit)) } in
          pr (P.str_of_msg res);
          P.write_9P_msg res server_fd;
          
        | P.T.Read (new_fid, (0, 0), count) ->
          let qid = { N.path = 1; N.vers = 0; N.typ = N.QTFile } in
          let entry = N.mk_dir_entry "foo.txt" qid (0o500, N.DMFile) in
          let str = P.pdir_entry entry in
          let len = String.length str in
          let res = { req with P.typ = P.R (P.R.Read str) } in
          pr (P.str_of_msg res);
          P.write_9P_msg res server_fd;

        | P.T.Read (new_fid, (_56TODO, _), count) ->
          let res = { req with P.typ = P.R (P.R.Read "") } in
          pr (P.str_of_msg res);
          P.write_9P_msg res server_fd;

        | _ -> raise Todo
        )
      | P.R x ->
        failwith "got a response request"
      )
    done
  )

let _ =
  main ()
