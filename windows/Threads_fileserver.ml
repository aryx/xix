(*s: Threads_fileserver.ml *)
(* Copyright 2017-2026 Yoann Padioleau, see copyright.txt *)
open Common

module D = Device
module N = Plan9
module P9 = Protocol_9P
module T = P9.Request
module R = P9.Response

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* ??? *)

(*****************************************************************************)
(* Constants *)
(*****************************************************************************)
(*s: constant [[Threads_fileserver.all_devices]] *)
let all_devices = [
  File.WinName , Virtual_draw.dev_winname;
  File.Mouse   , Virtual_mouse.dev_mouse;
  File.Cons    , Virtual_cons.dev_cons;
  File.ConsCtl , Virtual_cons.dev_consctl;
  
  File.WinId   , Dev_wm.dev_winid;
  File.Text    , Dev_textual_window.dev_text;
]
(*e: constant [[Threads_fileserver.all_devices]] *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
(*s: function [[Threads_fileserver.device_of_devid]] *)
let device_of_devid (devid : File.devid) : Device.t =
  try 
    List.assoc devid all_devices
  with Not_found ->
    raise (Impossible (spf "all_devices is not correctly set; missing code %d"
             (File.int_of_filecode (File.File devid))))
(*e: function [[Threads_fileserver.device_of_devid]] *)

(*s: constant [[Threads_fileserver.toplevel_entries]] *)
let toplevel_entries =
  all_devices |> List.map (fun (devid, dev) ->
    File.{
      name = dev.D.name;
      code = File.File devid;
      type_ = Plan9.QTFile;
      perm = dev.D.perm;
    }
  )
(*e: constant [[Threads_fileserver.toplevel_entries]] *)

(*s: function [[Threads_fileserver.answer]] *)
let answer (fs : Fileserver.t) (res : P9.message) =
  if !Globals.debug_9P
  then Logs.debug (fun m -> m "%s" (P9.str_of_msg res));

  P9.write_9P_msg res fs.server_fd
(*e: function [[Threads_fileserver.answer]] *)

(*s: function [[Threads_fileserver.error]] *)
let error fs req str =
  let res = { req with P9.typ = P9.R (R.Error str) } in
  answer fs res
(*e: function [[Threads_fileserver.error]] *)

(*s: function [[Threads_fileserver.check_fid]] *)
(* less: could be check_and_find_fid to factorize more code in dispatch() *)
let check_fid op fid (fs : Fileserver.t) =
  (* stricter: *)
  if not (Hashtbl.mem fs.fids fid)
  then failwith (spf "%s: unknown fid %d" op fid)
(*e: function [[Threads_fileserver.check_fid]] *)
  
(*****************************************************************************)
(* Dispatch *)
(*****************************************************************************)
(*s: constant [[Threads_fileserver.first_message]] *)
(* for Version *)
let first_message = ref true
(*e: constant [[Threads_fileserver.first_message]] *)

(*s: function [[Threads_fileserver.dispatch]] *)
let dispatch (fs : Fileserver.t) (req : P9.message) (request_typ : P9.Request.t) =
  match request_typ with
  (*s: [[Threads_fileserver.dispatch()]] match [[request_typ]] cases *)
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
      fs.message_size <- msize;
      answer fs {req with P9.typ = P9.R (R.Version (msize, str)) }
    )
  (*x: [[Threads_fileserver.dispatch()]] match [[request_typ]] cases *)
  (* Attach *)
  | T.Attach (rootfid, _auth_fid_opt, uname, aname) ->
    (* stricter: *)
    if Hashtbl.mem fs.fids rootfid
    then failwith (spf "Attach: fid already used: %d" rootfid);

    (match () with
    | _ when uname <> fs.user ->
      error fs req (spf "permission denied, %s <> %s" uname fs.user)
    | _ ->
    (* less: could do that in a worker thread (if use qlock) *)
    (* less: newlymade, qlock all *)
    (try
       let wid = int_of_string aname in
       let w = Hashtbl.find Globals.windows wid in

       let entry = File.root_entry in
       let file_id = entry.code, wid in
       let qid = File.qid_of_fileid file_id entry.type_ in

       let file = File.{
         fid = rootfid; 
         qid = qid; 
         entry = entry;
         opened = None;
         win = w;
       } in
       Hashtbl.add fs.fids rootfid file;
       answer fs {req with P9.typ = P9.R (R.Attach qid) }
     with _exn ->
        error fs req (spf "unknown id in attach: %s" aname)
    (* less: incref, qunlock *)
    ))
  (*x: [[Threads_fileserver.dispatch()]] match [[request_typ]] cases *)
  (* Walk *)
  | T.Walk (fid, newfid_opt, xs) ->
    check_fid "walk" fid fs;
    let file : File.t = Hashtbl.find fs.fids fid in
    let wid = file.win.id in
    (match file.opened, newfid_opt with
    | Some _, _ ->
      error fs req "walk of open file"
    (* stricter? failwith or error? *)
    | _, Some newfid when Hashtbl.mem fs.fids newfid ->
      error fs req (spf "clone to busy fid: %d" newfid)
    | _ when List.length xs > P9.max_welem ->
      error fs req (spf "name too long: [%s]" (String.concat ";" xs))
    | _ ->
      let file =
        match newfid_opt with
        | None -> file
        | Some newfid ->
          (* clone *)
          (* less: incref on file.w *)
          let newfile = { file with
            File.fid = newfid; 
            File.opened = None 
            (* todo: nrpart? *)
          } in
          Hashtbl.add fs.fids newfid newfile;
          newfile
      in
      let rec walk qid (entry : File.dir_entry_short) acc xs =
        match xs with
        | [] -> qid, entry, List.rev acc
        | x::xs ->
          if qid.N.typ <> N.QTDir
          (* will be catched below and transformed in an 9P Rerror message *)
          then failwith "not a directory";
          (match entry.code, x with
          | _Qwsys, ".." -> failwith "walk: Todo '..'"
          | File.Dir File.Root, x ->
            let entry = 
              toplevel_entries |> List.find (fun (entry : File.dir_entry_short)->
                       entry.name = x)
            in
            let file_id = entry.code, wid in
            let qid = File.qid_of_fileid file_id entry.type_ in
            (* continue with other path elements *)
            walk qid entry (qid::acc) xs
          (* todo: Wsys, snarf *)
          | _ -> 
            raise (Impossible "should be catched by 'not a directory' above")
          )
      in
      (try 
        let final_qid, final_entry, qids = 
          walk file.qid file.entry [] xs
        in
        file.qid <- final_qid;
        file.entry <- final_entry;
        answer fs { req with P9.typ = P9.R (R.Walk qids) }
      with exn ->
        newfid_opt |> Option.iter (fun newfid ->
          Hashtbl.remove fs.fids newfid
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
  (*x: [[Threads_fileserver.dispatch()]] match [[request_typ]] cases *)
  (* Open *)
  | T.Open (fid, flags) ->
    check_fid "open" fid fs;
    let file = Hashtbl.find fs.fids fid in
    let w = file.win in
    (* less: OTRUNC | OCEXEC | ORCLOSE, and remove DMDIR| DMAPPEND from perm *)
    let perm = file.entry.perm in
    if flags.x || (flags.r && not perm.r)
               || (flags.w && not perm.w)
    then error fs req "permission denied"
    else
      if w.deleted
      then error fs req "window deleted"
      else 
      (* less: could do that in a worker thread *)
      (try 
         (match file.entry.code with
         | File.File devid ->
           let dev = device_of_devid devid in
           dev.D.open_ w
         | File.Dir _dir ->
           (* todo: nothing to do for dir? ok to open a dir? *)
           ()
         );
         file.opened <- Some flags;
         let iounit = fs.message_size - P9.io_header_size in
         answer fs { req with P9.typ = P9.R (R.Open (file.qid, iounit)) }
       with Device.Error str ->
         error fs req str
      )
  (*x: [[Threads_fileserver.dispatch()]] match [[request_typ]] cases *)
  (* Clunk *)
  | T.Clunk (fid) ->
    check_fid "clunk" fid fs;
    let file = Hashtbl.find fs.fids fid in
    (match file.opened with
    | Some _flags ->
      (match file.entry.code with
      | File.File devid ->
        let dev = device_of_devid devid in
        dev.D.close file.win
        (* todo: wclose? *)
      | File.Dir _ ->
        ()
      )
    (* stricter? can clunk a file not opened? *)
    | None ->
      (* todo: winclosechan *)
      ()
    );
    Hashtbl.remove fs.fids fid;
    answer fs { req with P9.typ = P9.R (R.Clunk) }
  (*x: [[Threads_fileserver.dispatch()]] match [[request_typ]] cases *)
  (* Read *)
  | T.Read (fid, offset, count) ->
    check_fid "read" fid fs;
    let file = Hashtbl.find fs.fids fid in
    let w = file.win in

    (match file.entry.code with
    | _ when w.deleted ->
      error fs req "window deleted"
    | File.File devid ->
      (* a worker thread (less: opti: arena of workers? *)
      Thread.create (fun () ->
       (* less: getclock? *)
       (try 
         let dev = device_of_devid devid in
         let data = dev.D.read_threaded offset count w in
         answer fs { req with P9.typ = P9.R (R.Read data) }
       with Device.Error str ->
         error fs req str
      )) () |> ignore
    | File.Dir _ ->
      failwith "TODO: readdir"
    )
  (*x: [[Threads_fileserver.dispatch()]] match [[request_typ]] cases *)
  (* Write *)
  | T.Write (fid, offset, data) ->
    check_fid "write" fid fs;
    let file = Hashtbl.find fs.fids fid in
    let w = file.win in

    (match file.entry.code with
    | _ when w.deleted ->
      error fs req "window deleted"
    | File.File devid ->
      (* a worker thread (less: opti: arena of workers? *)
      Thread.create (fun () ->
       (try 
         let dev = device_of_devid devid in
         (* less: case where want to let device return different count? *)
         let count = String.length data in
         dev.D.write_threaded offset data w;
         answer fs { req with P9.typ = P9.R (R.Write count) }
       with Device.Error str ->
         error fs req str
      )) () |> ignore
    | File.Dir _ ->
      raise (Impossible "kernel should not call write on fid of a directory")
    )
  (*x: [[Threads_fileserver.dispatch()]] match [[request_typ]] cases *)
  (* Stat *)
  | T.Stat (fid) ->
    check_fid "stat" fid fs;
    let file = Hashtbl.find fs.fids fid in
    let short = file.entry in
    (* less: getclock *)
    let clock = 0 in
    let dir_entry = {
      N.name = short.name;
      (* less: adjust version for snarf *)
      N.qid = file.qid;
      N.mode = (N.int_of_perm_property short.perm, 
        match file.qid.N.typ with
        | N.QTDir -> N.DMDir
        | N.QTFile -> N.DMFile
      );
      N.length = 0; (*/* would be nice to do better */*)

      N.atime = clock;
      N.mtime = clock;

      N.uid = fs.user;
      N.gid = fs.user;
      N.muid = fs.user;

      N._typ = 0;
      N._dev = 0;
    }
    in
    answer fs { req with P9.typ = P9.R (R.Stat dir_entry) }
  (*x: [[Threads_fileserver.dispatch()]] match [[request_typ]] cases *)
  (* Other *)
  | T.Create _ 
  | T.Remove _
  | T.Wstat _
      -> error fs req "permission denied"
  (*x: [[Threads_fileserver.dispatch()]] match [[request_typ]] cases *)
  (* todo: handle those one? *)
  | T.Flush _
  | T.Auth _
    -> 
    failwith (spf "TODO: req = %s" (P9.str_of_msg req))
  (*e: [[Threads_fileserver.dispatch()]] match [[request_typ]] cases *)
(*e: function [[Threads_fileserver.dispatch]] *)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)
(*s: function [[Threads_fileserver.thread]] *)
(* the master *)
let thread (fs : Fileserver.t) =
  (* less: threadsetname *)
  
  while true do
    let req : P9.message = P9.read_9P_msg fs.server_fd in

    (*s: [[Threads_fileserver.thread]] in loop, debug *)
    (* todo: should exit the whole proc if error *)
    if !Globals.debug_9P
    then Logs.debug (fun m -> m "%s" (P9.str_of_msg req));
    (*e: [[Threads_fileserver.thread]] in loop, debug *)

    (match req.typ with
    | P9.T x -> dispatch fs req x
    | P9.R _x ->
      (* less: Ebadfcall *)
      raise (Impossible (spf "got a response request: %s" (P9.str_of_msg req)))
    );
    (* for Version first-message check *)
    first_message := false
  done
(*e: function [[Threads_fileserver.thread]] *)
(*e: Threads_fileserver.ml *)
