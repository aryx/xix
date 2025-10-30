open Common

module F = File
module D = Device
module FS = Fileserver
module W = Window
module N = Plan9
module P = Protocol_9P
module T = P.Request
module R = P.Response

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Constants *)
(*****************************************************************************)

let all_devices = [
  F.WinName , Virtual_draw.dev_winname;
  F.Mouse   , Virtual_mouse.dev_mouse;
  F.Cons    , Virtual_cons.dev_cons;
  F.ConsCtl , Virtual_cons.dev_consctl;
  
  F.WinId   , Dev_wm.dev_winid;
  F.Text    , Dev_textual_window.dev_text;
]

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let device_of_devid devid =
  try 
    List.assoc devid all_devices
  with Not_found ->
    raise (Impossible (spf "all_devices is not correctly set; missing code %d"
             (File.int_of_filecode (F.File devid))))

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
  then Logs.debug (fun m -> m "%s" (P.str_of_msg res));

  P.write_9P_msg res fs.FS.server_fd

let error fs req str =
  let res = { req with P.typ = P.R (R.Error str) } in
  answer fs res

(* less: could be check_and_find_fid to factorize more code in dispatch() *)
let check_fid op fid fs =
  (* stricter: *)
  if not (Hashtbl.mem fs.FS.fids fid)
  then failwith (spf "%s: unknown fid %d" op fid)
  

(*****************************************************************************)
(* Dispatch *)
(*****************************************************************************)

(* for Version *)
let first_message = ref true

let dispatch fs req request_typ =
  match request_typ with
  (* Version *)
  | T.Version (msize, str) -> 
    (match () with
    | _ when not !first_message ->
      error fs req "version: request not first message"
    | _ when msize < 256 ->
      error fs req "version: message size too small";
    | _ when str <> "9P2000" ->
      error fs req "version: unrecognized 9P version";
    | _ ->
      fs.FS.message_size <- msize;
      answer fs {req with P.typ = P.R (R.Version (msize, str)) }
    )

  (* Attach *)
  | T.Attach (rootfid, _auth_fid_opt, uname, aname) ->
    (* stricter: *)
    if Hashtbl.mem fs.FS.fids rootfid
    then failwith (spf "Attach: fid already used: %d" rootfid);

    (match () with
    | _ when uname <> fs.FS.user ->
      error fs req (spf "permission denied, %s <> %s" uname fs.FS.user)
    | _ ->
    (* less: could do that in a worker thread (if use qlock) *)
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
       answer fs {req with P.typ = P.R (R.Attach qid) }
     with _exn ->
        error fs req (spf "unknown id in attach: %s" aname)
    (* less: incref, qunlock *)
    ))

  (* Walk *)
  | T.Walk (fid, newfid_opt, xs) ->
    check_fid "walk" fid fs;
    let file = Hashtbl.find fs.FS.fids fid in
    let wid = file.F.w.W.id in
    (match file, newfid_opt with
    | { F.opened = Some _; _ }, _ ->
      error fs req "walk of open file"
    (* stricter? failwith or error? *)
    | _, Some newfid when Hashtbl.mem fs.FS.fids newfid ->
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
          let newfile = { file with 
            F.fid = newfid; 
            F.opened = None 
            (* todo: nrpart? *)
          } in
          Hashtbl.add fs.FS.fids newfid newfile;
          newfile
      in
      let rec walk qid entry acc xs =
        match xs with
        | [] -> qid, entry, List.rev acc
        | x::xs ->
          if qid.N.typ <> N.QTDir
          (* will be catched below and transformed in an 9P Rerror message *)
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
        answer fs { req with P.typ = P.R (R.Walk qids) }
      with exn ->
        newfid_opt |> Option.iter (fun newfid ->
          Hashtbl.remove fs.FS.fids newfid
        );
        (match exn with
        (* this can happen many times because /dev is a union-mount so
         * we get walk requests also for /dev/draw/... and other devices
         *)
        | Not_found -> 
          error fs req "file does not exist"
        | Failure s when s = "not a directory" ->
          error fs req "not a directory"
        (* internal error then *)
        | _ -> raise exn
        )
      )
    )

  (* Open *)
  | T.Open (fid, flags) ->
    check_fid "open" fid fs;
    let file = Hashtbl.find fs.FS.fids fid in
    let w = file.F.w in
    (* less: OTRUNC | OCEXEC | ORCLOSE, and remove DMDIR| DMAPPEND from perm *)
    (match flags, file.F.entry.F.perm with
    | { N.x = true; _}, _ 
    | { N.r = true; _}, { N.r = false; _}
    | { N.w = true; _}, { N.w = false; _}
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
         | F.Dir _dir ->
           (* todo: nothing to do for dir? ok to open a dir? *)
           ()
         );
         file.F.opened <- Some flags;
         let iounit = fs.FS.message_size - P.io_header_size in
         answer fs { req with P.typ = P.R (R.Open (file.F.qid, iounit)) }
       with Device.Error str ->
         error fs req str
      )
    )

  (* Clunk *)
  | T.Clunk (fid) ->
    check_fid "clunk" fid fs;
    let file = Hashtbl.find fs.FS.fids fid in
    (match file.F.opened with
    | Some _flags ->
      (match file.F.entry.F.code with
      | F.File devid ->
        let dev = device_of_devid devid in
        dev.D.close file.F.w
        (* todo: wclose? *)
      | F.Dir _ ->
        ()
      )
    (* stricter? can clunk a file not opened? *)
    | None ->
      (* todo: winclosechan *)
      ()
    );
    Hashtbl.remove fs.FS.fids fid;
    answer fs { req with P.typ = P.R (R.Clunk) }

  (* Read *)
  | T.Read (fid, offset, count) ->
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
         answer fs { req with P.typ = P.R (R.Read (Bytes.of_string data)) }
       with Device.Error str ->
         error fs req str
      )) () |> ignore
    | F.Dir _ ->
      failwith "TODO: readdir"
    )

  (* Write *)
  | T.Write (fid, offset, data) ->
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
         let count = Bytes.length data in
         dev.D.write_threaded offset (Bytes.to_string data) w;
         answer fs { req with P.typ = P.R (R.Write count) }
       with Device.Error str ->
         error fs req str
      )) () |> ignore
    | F.Dir _ ->
      raise (Impossible "kernel should not call write on fid of a directory")
    )
  (* Stat *)
  | T.Stat (fid) ->
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
    answer fs { req with P.typ = P.R (R.Stat dir_entry) }

  (* Other *)
  | T.Create _ 
  | T.Remove _
  | T.Wstat _
      -> error fs req "permission denied"

  (* todo: handle those one? *)
  | T.Flush _
  | T.Auth _
    -> 
    failwith (spf "TODO: req = %s" (P.str_of_msg req))

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

(* the master *)
let thread fs =
  (* less: threadsetname *)
  
  while true do
    let req = P.read_9P_msg fs.FS.server_fd in

    (* todo: should exit the whole proc if error *)
    if !Globals.debug_9P
    then Logs.debug (fun m -> m "%s" (P.str_of_msg req));

    (match req.P.typ with
    | P.T x -> dispatch fs req x
    | P.R _x ->
      (* less: Ebadfcall *)
      raise (Impossible (spf "got a response request: %s" (P.str_of_msg req)))
    );
    (* for Version first-message check *)
    first_message := false
  done
