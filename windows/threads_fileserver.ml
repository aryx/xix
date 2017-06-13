open Common

module F = File
module D = Device
module FS = Fileserver
module N = Plan9
module P = Protocol_9P
module W = Window

let all_devices = [
  F.Winname , Virtual_draw.dev_winname;
  F.Mouse   , Virtual_mouse.dev_mouse;
  F.Cons    , Virtual_cons.dev_cons;
  F.Consctl , Virtual_cons.dev_consctl;
]

let device_of_devid devid =
  List.assoc devid all_devices

let toplevel_entries =
  all_devices |> List.map (fun (devid, dev) ->
    { F.name = dev.D.name;
      F.code = F.File devid;
      F.type_ = Plan9.QTFile;
      F.perm = dev.D.perm;
    }
  )


let answer fs res =
  if !Globals.debug_9P
  then pr (P.str_of_msg res);

  P.write_9P_msg res fs.FS.server_fd

let error fs req str =
  let res = { req with P.typ = P.R (P.R.Error str) } in
  answer fs res

let check_fid op fid fs =
  (* stricter: *)
  if not (Hashtbl.mem fs.FS.fids fid)
  then failwith (spf "%s: unknown fid %d" op fid)
  

let first_message = ref true

let dispatch fs req request_typ =
  match request_typ with
  (* Version *)
  | P.T.Version (msize, str) -> 
    (match () with
    | _ when not !first_message ->
      error fs req "version: request not first message"
    | _ when msize < 256 ->
      error fs req "version: message size too small";
    | _ when str <> "9P2000" ->
      error fs req "version: unrecognized 9P version";
    | _ ->
      fs.FS.message_size <- msize;
      answer fs {req with P.typ = P.R (P.R.Version (msize, str)) }
    )

  (* Attach *)
  | P.T.Attach (rootfid, auth_fid_opt, uname, aname) ->
    (* stricter: *)
    if Hashtbl.mem fs.FS.fids rootfid
    then failwith (spf "Attach: fid already used: %d" rootfid);

    (match () with
    | _ when uname <> fs.FS.user ->
      error fs req (spf "permission denied, %s <> %s" uname fs.FS.user)
    | _ ->
    (* less: could do that in a worker thread *)
    (* less: newlymade, qlock all *)
    (try
       let wid = int_of_string aname in
       let w = Hashtbl.find Globals.windows wid in
       let entry = File.root_entry in
       let file_id = entry.F.code, wid in
       let qid = File.qid_of_fileid file_id entry.F.type_ in
       let file = { 
         F.fid = rootfid; 
         F.qid = qid; 
         F.entry = entry;
         F.opened = None;
         F.w = w;
       } in
       Hashtbl.add fs.FS.fids rootfid file;
       answer fs {req with P.typ = P.R (P.R.Attach qid) }
     with exn ->
        error fs req (spf "unknown id in attach: %s" aname)
    (* less: incref, qunlock *)
    )
    )

  (* Walk *)
  | P.T.Walk (fid, newfid_opt, xs) ->
    check_fid "walk" fid fs;

    let file = Hashtbl.find fs.FS.fids fid in
    let wid = file.F.w.W.id in
    (match file, newfid_opt with
    | { F.opened = Some _ }, _ ->
      error fs req "walk of open file"
    | _, Some newfid when Hashtbl.mem fs.FS.fids newfid ->
      (* stricter? failwith or error? *)
      error fs req (spf "clone to busy fid: %d" newfid)
    | _ when List.length xs > P.max_welem ->
      error fs req (spf "name too long: [%s]" (String.concat ";" xs))
    | _ ->
      let file =
        match newfid_opt with
        | None -> file
        | Some newfid ->
          (* clone *)
          (* less: incref on file.w *)
          let newfile = { file with F.fid = newfid; F.opened = None } in
          Hashtbl.add fs.FS.fids newfid newfile;
          newfile
      in
      let rec walk qid entry acc xs =
        match xs with
        | [] -> qid, entry, List.rev acc
        | x::xs ->
          if qid.N.typ <> N.QTDir
          (* will be catched below and transformed in an 9P Error message *)
          then failwith "not a directory";
          (match entry.F.code, x with
          | _Qwsys, ".." -> failwith "walk: Todo '..'"
          | F.Dir (F.Root), x ->
            let entry = 
              toplevel_entries |> List.find (fun entry -> entry.F.name = x)
            in
            let file_id = entry.F.code, wid in
            let qid = File.qid_of_fileid file_id entry.F.type_ in
            (* continue with other path elements *)
            walk qid entry (qid::acc) xs
            (* todo: Wsys, snarf *)
          | _ -> 
            raise (Impossible "should be catched by 'not a directory' above")
          )
      in
      (try 
        let final_qid, final_entry, qids = 
          walk file.F.qid file.F.entry [] xs
        in
        file.F.qid <- final_qid;
        file.F.entry <- final_entry;
        answer fs { req with P.typ = P.R (P.R.Walk qids) }
      with exn ->
        newfid_opt |> Common.if_some (fun newfid ->
          Hashtbl.remove fs.FS.fids newfid
        );
        (match exn with
        (* this can happen many times because /dev is union-mount so
         * we get requests also for /dev/draw/... and other devices
         *)
        | Not_found -> 
          error fs req "file does not exist"
        | Failure "not a directory" ->
          error fs req "not a directory"
        | _ -> raise exn
        )
      )
    )

  (* Open *)
  | P.T.Open (fid, flags) ->
    check_fid "open" fid fs;
    let file = Hashtbl.find fs.FS.fids fid in
    let w = file.F.w in
    (* less: OTRUNC | OCEXEC | ORCLOSE, and remove DMDIR| DMAPPEND from perm *)
    (match flags, file.F.entry.F.perm with
    | { N.x = true}, _ 
    | { N.r = true}, { N.r = false}
    | { N.w = true}, { N.w = false}
      -> error fs req "permission denied"
    | _, _ when w.W.deleted ->
      error fs req "window deleted"
    | _ ->
      (* less: could do that in a worker thread *)
      (try 
         (match file.F.entry.F.code with
         | F.File devid ->
           let dev = device_of_devid devid in
           dev.D.open_ w
         | F.Dir dir ->
           ()
         );
         file.F.opened <- Some flags;
         let iounit = fs.FS.message_size - P.io_header_size in
         answer fs { req with P.typ = P.R (P.R.Open (file.F.qid, iounit)) }
       with Device.Error str ->
         error fs req str
      )
    )

  (* Clunk *)
  | P.T.Clunk (fid) ->
    check_fid "clunk" fid fs;
    let file = Hashtbl.find fs.FS.fids fid in
    (match file.F.opened with
    (* todo? can clunk unopened file?? *)
    | Some _flags ->
      (match file.F.entry.F.code with
      | F.File devid ->
        let dev = device_of_devid devid in
        dev.D.close file.F.w
        (* todo: wclose? *)
      | F.Dir _ ->
        ()
      )
    | None ->
      (* todo: winclosechan *)
      ()
    );
    Hashtbl.remove fs.FS.fids fid;
    answer fs { req with P.typ = P.R (P.R.Clunk) }

  (* Read *)
  | P.T.Read (fid, offset, count) ->
    check_fid "read" fid fs;
    let file = Hashtbl.find fs.FS.fids fid in
    let w = file.F.w in

    (match file.F.entry.F.code with
    | _ when w.W.deleted ->
      error fs req "window deleted"
    | F.File devid ->
      (* a worker thread (less: opti: arena of workers? *)
      Thread.create (fun () ->
       (* less: getclock? *)
       (try 
         let dev = device_of_devid devid in
         let data = dev.D.read_threaded offset count w in
         answer fs { req with P.typ = P.R (P.R.Read data) }
       with 
         | Device.Error str ->
           error fs req str
      )) () |> ignore
    | F.Dir _ ->
      failwith "TODO: readdir"
    )

  (* Write *)
  | P.T.Write (fid, offset, data) ->
    check_fid "write" fid fs;
    let file = Hashtbl.find fs.FS.fids fid in
    let w = file.F.w in

    (match file.F.entry.F.code with
    | _ when w.W.deleted ->
      error fs req "window deleted"
    | F.File devid ->
      (* a worker thread (less: opti: arena of workers? *)
      Thread.create (fun () ->
       (try 
         let dev = device_of_devid devid in
         (* less: case where want to let device return different count? *)
         let count = String.length data in
         dev.D.write_threaded offset data w;
         answer fs { req with P.typ = P.R (P.R.Write count) }
       with 
         | Device.Error str ->
           error fs req str
      )) () |> ignore
    | F.Dir _ ->
      raise (Impossible "kernel should not call write on fid of a directory")
    )
  (* Stat *)
  | P.T.Stat (fid) ->
    check_fid "stat" fid fs;
    let file = Hashtbl.find fs.FS.fids fid in
    let short = file.F.entry in
    (* less: getclock *)
    let clock = 0 in
    let dir_entry = {
      N.name = short.F.name;
      (* less: adjust version for snarf *)
      N.qid = file.F.qid;
      N.mode = (N.int_of_perm_property short.F.perm, 
        match file.F.qid.N.typ with
        | N.QTDir -> N.DMDir
        | N.QTFile -> N.DMFile
      );
      N.length = 0; (*/* would be nice to do better */*)

      N.atime = clock;
      N.mtime = clock;

      N.uid = fs.FS.user;
      N.gid = fs.FS.user;
      N.muid = fs.FS.user;

      N._typ = 0;
      N._dev = 0;
    }
    in
    answer fs { req with P.typ = P.R (P.R.Stat dir_entry) }

  (* Other *)
  | P.T.Create _ 
  | P.T.Remove _
  | P.T.Wstat _
      -> error fs req "permission denied"

  (* todo: handle those one? *)
  | P.T.Flush _
  | P.T.Auth _
    -> 
    failwith (spf "TODO: req = %s" (P.str_of_msg req))

(* the master *)
let thread fs =
  (* less: threadsetname *)
  
  while true do
    (* less: care about messagesize? *)
    let req = P.read_9P_msg fs.FS.server_fd in
    (* todo: should exit the whole proc if error *)
    if !Globals.debug_9P
    then pr (P.str_of_msg req);

    (match req.P.typ with
    | P.T x -> dispatch fs req x
    | P.R x ->
      (* less: Ebadfcall *)
      raise (Impossible (spf "got a response request: %s" (P.str_of_msg req)))
    );
    first_message := false
  done
