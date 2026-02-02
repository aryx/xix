open Common

module N = Plan9
module P = Protocol_9P

let main (caps : < Cap.mount; Cap.stdout; ..>) =
  let (clients_fd, server_fd) = Unix.pipe () in
  let pr = Console.print caps in
  
  let pid = Unix.fork () in
  (match pid with
  (* child, client *)
  | 0 ->
    (try 
      Plan9.mount caps clients_fd (-1) (Fpath.v "/mnt/wsys") Plan9.MRepl "win1" |> ignore;
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
        | P.Request.Version (msize, str) ->
          let res = { req with P.typ = P.R (P.Response.Version (msize, str)) } in
          pr (P.str_of_msg res);
          P.write_9P_msg res server_fd;
          ()
        | P.Request.Attach (rootfid, afid, uname, aname) -> 
          let qid = { N.path = 0; N.vers = 0; N.typ = N.QTDir } in
          let res = { req with P.typ = P.R (P.Response.Attach qid) } in
          pr (P.str_of_msg res);
          P.write_9P_msg res server_fd;

        (* for Unix.opendir *)
        | P.Request.Walk (rootfid, new_fid, []) -> 
          let res = { req with P.typ = P.R (P.Response.Walk []) } in
          pr (P.str_of_msg res);
          P.write_9P_msg res server_fd;

        | P.Request.Stat (new_fid) -> 
          let qid = { N.path = 0; N.vers = 0; N.typ = N.QTDir } in
          let entry = N.mk_dir_entry "/" qid (0o500, N.DMDir) in
          let res = { req with P.typ = P.R (P.Response.Stat entry) } in
          pr (P.str_of_msg res);
          P.write_9P_msg res server_fd;

        | P.Request.Clunk (new_fid) -> 
          let res = { req with P.typ = P.R (P.Response.Clunk) } in
          pr (P.str_of_msg res);
          P.write_9P_msg res server_fd;

        (* for Unix.readdir *)
        | P.Request.Open (new_fid, flag) ->
          let qid = { N.path = 0; N.vers = 0; N.typ = N.QTDir } in
          let iounit = 8192 in
          let res = { req with P.typ = P.R (P.Response.Open (qid, iounit)) } in
          pr (P.str_of_msg res);
          P.write_9P_msg res server_fd;
          
        | P.Request.Read (new_fid, 0 (* Was (0,0)*), count) ->
          let qid = { N.path = 1; N.vers = 0; N.typ = N.QTFile } in
          let entry = N.mk_dir_entry "foo.txt" qid (0o500, N.DMFile) in
          let str = P.pdir_entry entry in
          let len = String.length str in
          let res = { req with P.typ = P.R (P.Response.Read str) } in
          pr (P.str_of_msg res);
          P.write_9P_msg res server_fd;

        | P.Request.Read (new_fid, _56TODO (* was pair before*), count) ->
          let res = { req with P.typ = P.R (P.Response.Read "") } in
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
  Cap.main (fun caps -> main caps)

